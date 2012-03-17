
namespace FMarkup

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Futility

module Prim =
  let elem tag (c : obj list) = 
    let e = {
      Tag           = tag
      NoEnd         = false
      Attributes    = ResizeArray ()
      Contents      = ResizeArray ()
      Classes       = ResizeArray ()
      Styles        = ResizeArray ()
    }
    e.Add c
    e
  let elemc tag (c : obj list) = 
    let e = {
      Tag           = tag
      NoEnd         = true
      Attributes    = ResizeArray ()
      Contents      = ResizeArray ()
      Classes       = ResizeArray ()
      Styles        = ResizeArray ()
    }
    e.Add c
    e
  let attr name value =
    let a = {
      Name          = name
      Contents      = ResizeArray ()
    }
    a.Contents.Add value
    a
  let raw value     = Raw value
  let enc str       = System.Web.HttpUtility.HtmlEncode str
  let encattr str   = System.Web.HttpUtility.HtmlAttributeEncode str
  let style name v  = {
    Name = name
    Value = v
  }
  let rule selector (styles : obj list) =
    let s = {
      Selector      = selector
      Styles        = ResizeArray ()
      Rules         = ResizeArray ()
    }
    s.Add styles
    s
  let cssClass name = { Class = name }

module Doctype =
  open Prim
  let html5 = raw "<!DOCTYPE html>"
  let html4strict = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
  let html4loose = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
  let html4frameset = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">"
  let xhtml1strict = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
  let xhtml1transitional = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
  let xhtml1frameset = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
  let xhtml11 = raw "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

module Enctype =
  let urlencoded = "application/x-www-form-urlencoded"
  let multipart = "multipart/form-data"
  
module Elem =
  open Prim
  // Html4
  let a c = elem "a" c
  let abbr c = elem "abbr" c
  let acronym c = elem "acronym" c
  let address c = elem "address" c
  let applet c = elem "applet" c
  let area c = elemc "area" c
  let b c = elem "b" c
  let base' c = elemc "base" c
  let basefont c = elemc "basefont" c
  let bdo c = elem "bdo" c
  let big c = elem "big" c
  let blockquote c = elem "blockquote" c
  let body c = elem "body" c
  let br = elemc "br" []
  let button c = elem "button" c
  let caption c = elem "caption" c
  let center c = elem "center" c
  let cite c = elem "cite" c
  let code c = elem "code" c
  let col c = elemc "col" c
  let colgroup c = elem "colgroup" c
  let dd c = elem "dd" c
  let del c = elem "del" c
  let dfn c = elem "dfn" c
  let dir c = elem "dir" c
  let div c = elem "div" c
  let dl c = elem "dl" c
  let dt c = elem "dt" c
  let em c = elem "em" c
  let fieldset c = elem "fieldset" c
  let font c = elem "font" c
  let form c = elem "form" c
  let frame c = elemc "frame" c
  let frameset c = elem "frameset" c
  let h1 c = elem "h1" c
  let h2 c = elem "h2" c
  let h3 c = elem "h3" c
  let h4 c = elem "h4" c
  let h5 c = elem "h5" c
  let h6 c = elem "h6" c
  let head c = elem "head" c
  let hr c = elemc "hr" c
  let html c = elem "html" c
  let i c = elem "i" c
  let iframe c = elem "iframe" c
  let img c = elemc "img" c
  let input c = elemc "input" c
  let ins c = elem "ins" c
  let isindex c = elemc "isindex" c
  let kbd c = elem "kbd" c
  let label c = elem "label" c
  let legend c = elem "legend" c
  let li c = elem "li" c
  let link c = elemc "link" c
  let map c = elem "map" c
  let menu c = elem "menu" c
  let meta c = elemc "meta" c
  let noframes c = elem "noframes" c
  let noscript c = elem "noscript" c
  let object' c = elem "object" c
  let ol c = elem "ol" c
  let optgroup c = elem "optgroup" c
  let option c = elem "option" c
  let p c = elem "p" c
  let param c = elemc "param" c
  let pre c = elem "pre" c
  let q c = elem "q" c
  let s c = elem "s" c
  let samp c = elem "samp" c
  let script c = elem "script" c
  let select c = elem "select" c
  let small c = elem "small" c
  let span c = elem "span" c
  let strike c = elem "strike" c
  let strong c = elem "strong" c
  let style c = elem "style" c
  let sub c = elem "sub" c
  let sup c = elem "sup" c
  let table c = elem "table" c
  let tbody c = elem "tbody" c
  let td c = elem "td" c
  let textarea c = elem "textarea" c
  let tfoot c = elem "tfoot" c
  let th c = elem "th" c
  let thead c = elem "thead" c
  let title c = elem "title" c
  let tr c = elem "tr" c
  let tt c = elem "tt" c
  let u c = elem "u" c
  let ul c = elem "ul" c
  let var c = elem "var" c  
  // Html5
  let section c = elem "section" c
  let article c = elem "article" c
  let aside c = elem "aside" c
  let hgroup c = elem "hgroup" c
  let header c = elem "header" c
  let footer c = elem "footer" c
  let nav c = elem "nav" c
  let figure c = elem "figure" c
  let figcaption c = elem "figcaption" c
  let video c = elem "video" c
  let audio c = elem "audio" c
  let embed c = elem "embed" c
  let mark c = elem "mark" c
  let progress c = elem "progress" c
  let meter c = elem "meter" c
  let time c = elem "time" c
  let ruby c = elem "ruby" c
  let rt c = elem "rt" c
  let rp c = elem "rp" c
  let canvas c = elem "canvas" c
  let command c = elem "command" c
  let details c = elem "details" c
  let datalist c = elem "datalist" c
  let keygen c = elem "keygen" c
  let output c = elem "output" c

module Attr =
  open Prim
  // Html4
  let abbrattr c = attr "abbr" c
  let acceptCharset c = attr "accept-charset" c
  let accept c = attr "accept" c
  let accesskey c = attr "accesskey" c
  let action c = attr "action" c
  let align c = attr "align" c
  let alink c = attr "alink" c
  let alt c = attr "alt" c
  let archive c = attr "archive" c
  let axis c = attr "axis" c
  let background c = attr "background" c
  let bgcolor c = attr "bgcolor" c
  let border c = attr "border" c
  let cellpadding c = attr "cellpadding" c
  let cellspacing c = attr "cellspacing" c
  let char c = attr "char" c
  let charoff c = attr "charoff" c
  let charset c = attr "charset" c
  let checked' c = attr "checked" c
  let citeattr c = attr "cite" c
  let class' c = attr "class" c
  let classid c = attr "classid" c
  let clear c = attr "clear" c
  let codeattr c = attr "code" c
  let codebase c = attr "codebase" c
  let codetype c = attr "codetype" c
  let color c = attr "color" c
  let cols c = attr "cols" c
  let colspan c = attr "colspan" c
  let compact c = attr "compact" c
  let content c = attr "content" c
  let coords c = attr "coords" c
  let data c = attr "data" c
  let datetime c = attr "datetime" c
  let declare c = attr "declare" c
  let defer c = attr "defer" c
  let dirattr c = attr "dir" c
  let disabled c = attr "disabled" c
  let enctype c = attr "enctype" c
  let face c = attr "face" c
  let for' c = attr "for" c
  let frameattr c = attr "frame" c
  let frameborder c = attr "frameborder" c
  let headers c = attr "headers" c
  let height c = attr "height" c
  let href c = attr "href" c
  let hreflang c = attr "hreflang" c
  let hspace c = attr "hspace" c
  let httpEquiv c = attr "http-equiv" c
  let id c = attr "id" c
  let ismap c = attr "ismap" c
  let labelattr c = attr "label" c
  let lang c = attr "lang" c
  let language c = attr "language" c
  let linkattr c = attr "link" c
  let longdesc c = attr "longdesc" c
  let marginheight c = attr "marginheight" c
  let marginwidth c = attr "marginwidth" c
  let maxlength c = attr "maxlength" c
  let media c = attr "media" c
  let method' c = attr "method" c
  let multiple c = attr "multiple" c
  let name c = attr "name" c
  let nohref c = attr "nohref" c
  let noresize c = attr "noresize" c
  let noshade c = attr "noshade" c
  let nowrap c = attr "nowrap" c
  let objectattr c = attr "object" c
  let onblur c = attr "onblur" c
  let onchange c = attr "onchange" c
  let onclick c = attr "onclick" c
  let ondblclick c = attr "ondblclick" c
  let onfocus c = attr "onfocus" c
  let onkeydown c = attr "onkeydown" c
  let onkeypress c = attr "onkeypress" c
  let onkeyup c = attr "onkeyup" c
  let onload c = attr "onload" c
  let onmousedown c = attr "onmousedown" c
  let onmousemove c = attr "onmousemove" c
  let onmouseout c = attr "onmouseout" c
  let onmouseover c = attr "onmouseover" c
  let onmouseup c = attr "onmouseup" c
  let onreset c = attr "onreset" c
  let onselect c = attr "onselect" c
  let onsubmit c = attr "onsubmit" c
  let onunload c = attr "onunload" c
  let profile c = attr "profile" c
  let prompt c = attr "prompt" c
  let readonly c = attr "readonly" c
  let rel c = attr "rel" c
  let rev c = attr" rev" c
  let rows c = attr "rows" c
  let rowspan c = attr "rowspan" c
  let rules c = attr "rules" c
  let scheme c = attr "scheme" c
  let scope c = attr "scope" c
  let scrolling c = attr "scrolling" c
  let selected c = attr "selected" c
  let shape c = attr "shape" c
  let size c = attr "size" c
  let spanattr c = attr "span" c
  let src c = attr "src" c
  let standby c = attr "standby" c
  let start c = attr "start" c
  let styleattr c = attr "style" c
  let summary c = attr "summary" c
  let tabindex c = attr "tabindex" c
  let target c = attr "target" c
  let text c = attr "text" c
  let titleattr c = attr "title" c
  let type' c = attr "type" c
  let usemap c = attr "usemap" c
  let valign c = attr "valign" c
  let value c = attr "value" c
  let valuetype c = attr "valuetype" c
  let version c = attr "version" c
  let vlink c = attr "vlink" c
  let vspace c = attr "vspace" c
  let width c = attr "width" c
  // Html5
  let ping c = attr "ping" c
  let autofocus c = attr "autofocus" c
  let placeholder c = attr "placeholder" c
  let formattr c = attr "form" c
  let required c = attr "required" c
  let autocomplete c = attr "autocomplete" c
  let min c = attr "min" c
  let max c = attr "max" c
  let pattern c = attr "pattern" c
  let step c = attr "step" c
  let list' c = attr "list" c
  let novalidate c = attr "novalidate" c
  let formaction c = attr "formaction" c
  let formenctype c = attr "formenctype" c
  let formmethod c = attr "formmethod" c
  let formnovalidate c = attr "formnovalidate" c
  let formtarget c = attr "formtarget" c
  let scoped c = attr "scoped" c
  let asyncattr c = attr "async" c
  let manifest c = attr "manifest" c
  let sizes c = attr "sizes" c
  let reversed c = attr "reversed" c
  let sandbox c = attr "sandbox" c
  let seamless c = attr "seamless" c
  let srcdoc c = attr "srcdoc" c
  let contenteditable c = attr "contenteditable" c
  let contextmenu c = attr "contextmenu" c
  let data' name c = attr ("data-" + name) c
  let aria name c = attr ("aria-" + name) c
  let draggable c = attr "draggable" c
  let hidden c = attr "hidden" c
  let role c = attr "role" c
  let spellcheck c = attr "spellcheck" c
  let onafterprint c = attr "onafterprint" c
  let onbeforeprint c = attr "onbeforeprint" c
  let onbeforeunload c = attr "onbeforeunload" c
  let onerror c = attr "onerror" c
  let onhaschange c = attr "onhaschange" c
  let onmessage c = attr "onmessage" c
  let onoffline c = attr "onoffline" c
  let ononline c = attr "ononline" c
  let onpagehide c = attr "onpagehide" c
  let onpageshow c = attr "onpageshow" c
  let onpopstate c = attr "onpopstate" c
  let onredo c = attr "onredo" c
  let onresize c = attr "onresize" c
  let onstorage c = attr "onstorage" c
  let onundo c = attr "onundo" c

module Ent =
  open Prim
  let tm = raw "&trade;"
  let nbsp = raw "&nbsp;"
  let copyright = raw "&copy;"

