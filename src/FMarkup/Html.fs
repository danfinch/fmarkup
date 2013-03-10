
namespace FMarkup

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Futility

module Prim =
  let inline elem tag (c : obj list) = 
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
  let inline elemc tag (c : obj list) = 
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
  let inline attr name value =
    let a = {
      Name          = name
      Contents      = ResizeArray ()
    }
    a.Contents.Add value
    a
  let inline raw value     = Raw value
  let inline enc str       = System.Web.HttpUtility.HtmlEncode str
  let inline encattr str   = System.Web.HttpUtility.HtmlAttributeEncode str
  let inline style name v  = {
    Name = name
    Value = v
  }
  let inline rule selector (styles : obj list) =
    let s = {
      Selector      = selector
      Styles        = ResizeArray ()
      Rules         = ResizeArray ()
    }
    s.Add styles
    s
  let inline cssClass name = { Class = name }

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
  let inline a c = elem "a" c
  let inline abbr c = elem "abbr" c
  let inline acronym c = elem "acronym" c
  let inline address c = elem "address" c
  let inline applet c = elem "applet inline" c
  let inline area c = elemc "area" c
  let inline b c = elem "b" c
  let inline base' c = elemc "base" c
  let inline basefont c = elemc "basefont" c
  let inline bdo c = elem "bdo" c
  let inline big c = elem "big" c
  let inline blockquote c = elem "blockquote" c
  let inline body c = elem "body" c
  let br = elemc "br" []
  let inline button c = elem "button" c
  let inline caption c = elem "caption" c
  let inline center c = elem "center" c
  let inline cite c = elem "cite" c
  let inline code c = elem "code" c
  let inline col c = elemc "col" c
  let inline colgroup c = elem "colgroup" c
  let inline dd c = elem "dd" c
  let inline del c = elem "del" c
  let inline dfn c = elem "dfn" c
  let inline dir c = elem "dir" c
  let inline div c = elem "div" c
  let inline dl c = elem "dl" c
  let inline dt c = elem "dt" c
  let inline em c = elem "em" c
  let inline fieldset c = elem "fieldset" c
  let inline font c = elem "font" c
  let inline form c = elem "form" c
  let inline frame c = elemc "frame" c
  let inline frameset c = elem "frameset" c
  let inline h1 c = elem "h1" c
  let inline h2 c = elem "h2" c
  let inline h3 c = elem "h3" c
  let inline h4 c = elem "h4" c
  let inline h5 c = elem "h5" c
  let inline h6 c = elem "h6" c
  let inline head c = elem "head" c
  let inline hr c = elemc "hr" c
  let inline html c = elem "html" c
  let inline i c = elem "i" c
  let inline iframe c = elem "iframe" c
  let inline img c = elemc "img" c
  let inline input c = elemc "input" c
  let inline ins c = elem "ins" c
  let inline isindex c = elemc "isindex" c
  let inline kbd c = elem "kbd" c
  let inline label c = elem "label" c
  let inline legend c = elem "legend" c
  let inline li c = elem "li" c
  let inline link c = elemc "link" c
  let inline map c = elem "map" c
  let inline menu c = elem "menu" c
  let inline meta c = elemc "meta" c
  let inline noframes c = elem "noframes" c
  let inline noscript c = elem "noscript" c
  let inline object' c = elem "object" c
  let inline ol c = elem "ol" c
  let inline optgroup c = elem "optgroup" c
  let inline option c = elem "option" c
  let inline p c = elem "p" c
  let inline param c = elemc "param" c
  let inline pre c = elem "pre" c
  let inline q c = elem "q" c
  let inline s c = elem "s" c
  let inline samp c = elem "samp" c
  let inline script c = elem "script" c
  let inline select c = elem "select" c
  let inline small c = elem "small" c
  let inline span c = elem "span" c
  let inline strike c = elem "strike" c
  let inline strong c = elem "strong" c
  let inline style c = elem "style" c
  let inline sub c = elem "sub" c
  let inline sup c = elem "sup" c
  let inline table c = elem "table" c
  let inline tbody c = elem "tbody" c
  let inline td c = elem "td" c
  let inline textarea c = elem "textarea" c
  let inline tfoot c = elem "tfoot" c
  let inline th c = elem "th" c
  let inline thead c = elem "thead" c
  let inline title c = elem "title" c
  let inline tr c = elem "tr" c
  let inline tt c = elem "tt" c
  let inline u c = elem "u" c
  let inline ul c = elem "ul" c
  let inline var c = elem "var" c  
  // Html5
  let inline section c = elem "section" c
  let inline article c = elem "article" c
  let inline aside c = elem "aside" c
  let inline hgroup c = elem "hgroup" c
  let inline header c = elem "header" c
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
  let data name c = attr ("data-" + name) c
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

