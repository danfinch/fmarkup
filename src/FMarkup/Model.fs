
namespace FMarkup

// todo: dynamic compilation
// todo: print DUs as strings/tuples

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Futility

type HtmlAttribute = {
  Name        : string
  Contents    : obj ResizeArray
}

type MarkupFormat =
  | HTML
  | XHTML

type CssStyle = {
  Name        : string
  Value       : obj
}

type CssClass = {
  Class       : string
}

type CssRule = {
  Selector    : string
  Styles      : CssStyle ResizeArray
  Rules       : CssRule ResizeArray
}
  with
    member self.Add (x : obj) =
      match x with
      | :? CssStyle as x -> self.Styles.Add x
      | :? CssRule as x -> self.Rules.Add x
      | :? Collections.IEnumerable as x -> for y in x do self.Add y
      | _ -> ()

type IHtmlWriter =
  abstract Write : TextWriter -> unit

module private Internal =
  let inline (|UnionType|_|) (t : Type) = if FSharpType.IsUnion t then Some t else None
  let inline (|TupleObject|_|) o =
    let t = o.GetType ()
    if Reflection.FSharpType.IsTuple t then Some o else None
  let inline (|OptionObject|_|) (o : 'a) =
    let t = o.GetType ()
    if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<option<_>> then Some (o :> obj)
    else None

open Internal

type HtmlContent =
  | Element   of HtmlElement
  | Text      of obj
  | Raw       of obj
  | Rule      of CssRule
  | Writer    of (TextWriter -> unit)

and HtmlElement = {
  Tag         : string
  NoEnd       : bool
  Attributes  : HtmlAttribute ResizeArray
  Contents    : HtmlContent ResizeArray
  Classes     : string ResizeArray
  Styles      : CssStyle ResizeArray 
}
  with
    member self.Add (x : obj) =
      match x with
      | null -> ()
      | :? string as x -> self.Contents.Add (Text x)
      | :? HtmlContent as x -> self.Contents.Add x
      | :? IHtmlWriter as x -> self.Contents.Add (Writer x.Write)
      | :? Collections.IEnumerable as x -> for y in x do self.Add y
      | :? HtmlAttribute as x -> self.Attributes.Add x
      | :? HtmlElement as x -> self.Contents.Add (Element x)
      | :? CssClass as x -> self.Classes.Add x.Class
      | :? CssStyle as x -> self.Styles.Add x
      | :? CssRule as x -> self.Contents.Add (Rule x)
      | TupleObject x -> for x in FSharpValue.GetTupleFields (x.GetType ()) do self.Add x
      | OptionObject x -> match x :?> _ option with Some x -> self.Add x | _ -> ()
      | _ -> self.Contents.Add (Text x)

type HtmlPrinter = {
  Format      : MarkupFormat
  CssUnit     : string
}
  with
    member self.PrintString (x : obj) =
      let sb = Text.StringBuilder ()
      use tw = new IO.StringWriter (sb)
      self.Print tw x
      sb.ToString ()
    member self.PrintRaw (x : obj) =
      Raw (self.PrintString x)
    member self.Print (w : TextWriter) (x : obj) =
      let printCssValue (v : obj) =
        match v with
        | :? int as v -> (v.ToString ()) + self.CssUnit :> obj
        | :? float as v -> (v.ToString ()) + self.CssUnit :> obj
        | _ -> v
      let getString (v : obj) =
        match v.GetType () with
        | UnionType t ->
          (fst (FSharpValue.GetUnionFields (v, t))).Name
        | _ -> string v
      match x with
      | null -> ()
      | :? string as x -> w.Write (Web.HttpUtility.HtmlEncode x)
      | :? HtmlContent as x ->
        match x with
        | Element x -> x |> self.Print w
        | Text x -> Web.HttpUtility.HtmlEncode (getString x, w)
        | Raw x -> w.Write x
        | Writer x -> x w
        | Rule x ->
          let rec printRule r pre =
            if not <| String.IsNullOrEmpty pre then
              w.Write pre
            w.Write r.Selector
            w.Write "{"
            for s in r.Styles do
              w.Write s.Name
              w.Write ':'
              w.Write (printCssValue s.Value)
              w.Write ';'
            w.Write "} "
            for sr in r.Rules do
              printRule sr (pre + r.Selector)
          printRule x ""
      | :? HtmlElement as e ->
        let attributes = ResizeArray<HtmlAttribute> (e.Attributes) 
        let autoclass = ref null
        w.Write '<'
        w.Write e.Tag
        if not (e.Styles |> Seq.isEmpty) then do
          let style = String.Join (";", e.Styles |> Seq.map (fun x -> String.Join (":", [x.Name :> obj; printCssValue x.Value])))
          let existing = attributes |> ResizeArray.filter (fun x -> x.Name = "style")
          if ResizeArray.isEmpty existing then do
            attributes.Add { Name = "style"; Contents = ResizeArray ([style :> obj]) }
          else do
            existing.[0].Contents.Add style
        if e.Tag.ToUpper() <> "STYLE" && (e.Contents |> Seq.exists (fun c -> match c with Rule x -> true | _ -> false)) then
          autoclass := "auto-" + Guid.NewGuid().ToString()
          e.Classes.Add !autoclass
        if not (e.Classes |> Seq.isEmpty) then do
          let cc = String.Join (" ", e.Classes |> ResizeArray.toArray)
          let existing = attributes |> ResizeArray.filter (fun x -> x.Name = "class")
          if ResizeArray.isEmpty existing then do
            attributes.Add { Name = "class"; Contents = ResizeArray ([cc :> obj]) }
          else do
            existing.[0].Contents.Add cc
        if not (ResizeArray.isEmpty attributes) then do
          w.Write ' '
          let mutable i = 0
          for a in attributes do
            w.Write a.Name
            w.Write '='
            w.Write '"'
            let contents = a.Contents |> ResizeArray.map (fun x ->
              System.Web.HttpUtility.HtmlAttributeEncode (getString x))
            w.Write (String.Join (" ", contents |> ResizeArray.toArray))
            w.Write '"'
            i <- i + 1
            if i < attributes.Count then do w.Write ' '
        if e.NoEnd then do
          if self.Format = XHTML then do
            w.Write " />"
          else do
            w.Write ">"
        else do
          w.Write ">"
          for c in e.Contents do
            match c with
            | Rule r ->
              if e.Tag.ToUpper () = "STYLE" then
                self.Print w c
              else
                let rule = { r with Selector = "." + !autoclass + " " + r.Selector }
                let elem = Element ({ Tag = "style"; NoEnd = false; Attributes = ResizeArray ();
                                    Classes = ResizeArray (); Styles = ResizeArray ();
                                    Contents = ResizeArray [ Rule rule ]
                                    })
                self.Print w elem
            | _ ->
              self.Print w c
          w.Write "</"
          w.Write e.Tag
          w.Write '>'
      | :? Collections.IEnumerable as x -> for y in x do self.Print w y
      | TupleObject x -> for y in FSharpValue.GetTupleFields (x.GetType ()) do self.Print w y
      | OptionObject x -> match x :?> _ option with Some x -> self.Print w x | _ -> ()
      | _ -> Web.HttpUtility.HtmlEncode (getString x, w)

type HtmlView = {
  InModel     : Type
  Method      : MethodInfo
}

module Views =
  let fromAssembly (assembly : Assembly) =
    let findPrint (t : Type) =
      let m = t.GetMethod "print"
      if notNull m && m.GetParameters().Length = 1 then
        Some {
          Method = m
          InModel = m.GetParameters().[0].ParameterType
        }
      else None
    assembly.GetTypes ()
    |> List.ofArray
    |> List.filter FSharpType.IsModule
    |> List.choose findPrint
  let fromAssemblies (assemblies : Assembly seq) =
    assemblies
    |> List.ofSeq
    |> List.map fromAssembly
    |> List.concat
