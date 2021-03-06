﻿
namespace FMarkup

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Futility

module Compact =
  open Prim
  open Elem
  open Attr
  let (~&) (o : 'a option) = o.IsSome // remove?
  let (!) (o : 'a option) = o.Value // remove?
  let (!!) (o : 'a option) = match o with | None -> null | Some(o) -> o :> obj // remove?
  let (++) (e : obj list -> HtmlElement) (a : 'a) = e %%[ a ] // remove?
  let where cond (fn : unit -> 'a) : obj = if cond then (fn () :> obj) else null
  let unless cond (fn : unit -> 'a) : obj = if cond then null else (fn () :> obj)
  let switch cond (t : unit -> 'a) (f : unit -> 'b) : obj = if cond then t () :> obj else f () :> obj
  let elem = elem
  let attr = attr
  let prop = style
  let raw = raw
  let cc = cssClass
  let html = html
  let head = head
  let meta = meta
  let link = link
  let title = title
  let style = style
  let rule = Prim.rule
  let body = body
  let frameset = frameset
  let frame = frame
  let iframe = iframe
  let a = a
  let div = div
  let span = span
  let p = p
  let table = table
  let tr = tr
  let th = th
  let td = td
  let thead = thead
  let tbody = tbody
  let rowspan = rowspan
  let colspan = colspan
  let h1 = h1
  let h2 = h2
  let h3 = h3
  let h4 = h4
  let h5 = h5
  let h6 = h6
  let br = br
  let form = form
  let input = input
  let button = button
  let textarea = textarea
  let hr = hr []
  let img = img
  let ul = ul
  let ol = ol
  let li = li
  let em = em
  let strong = strong
  let cite = cite  
  let href = href
  let target = target
  let src = src
  let tip = titleattr
  let meth = method'
  let action = action
  let rows = rows
  let cols = cols
  let type' = type'
  let rel = rel
  let method' = method'
  let name = name
  let value = value
  let alt = alt
  let checked' = checked'
  let wi = Css.width
  let hi = Css.height
  let top = Css.top
  let left = Css.left
  let right = Css.right
  let bottom = Css.bottom
  let absolute = Css.position.absolute
  let relative = Css.position.relative
  let minwi = Css.minWidth
  let minhi = Css.minHeight
  let maxwi = Css.maxWidth
  let maxhi = Css.maxHeight
  let lihi = Css.line.height
  let inl = Css.display.inline'
  let blo = Css.display.block
  let inb = Css.display.inlineBlock
  let flr = Css.float.right
  let fll = Css.float.left
  let fg = Css.color
  let bg = Css.background.color
  let bgi u = Css.background.image ("url('" + u + "')")
  let bgpos p = Css.background.position p
  module bgr =
    let repeat = Prim.style "background-repeat" "repeat"
    let repeatX = Prim.style "background-repeat" "repeat-x"
    let repeatY = Prim.style "background-repeat" "repeat-y"
    let noRepeat = Prim.style "background-repeat" "no-repeat"
    let inherit' = Prim.style "background-repeat" "inherit"
  let fs = Css.font.size
  let ff = Css.font.family
  let fw = Css.font.weight.value
  let normal = Css.font.weight.normal
  let bold = Css.font.weight.bold  
  let italic = Css.font.style.italic
  let lt = Css.text.align.left
  let ct = Css.text.align.center
  let rt = Css.text.align.right
  let jt = Css.text.align.justify
  let mg = Css.margin.value
  let mgl = Css.margin.left
  let mgr = Css.margin.right
  let mgt = Css.margin.top
  let mgb = Css.margin.bottom
  let mgh v = [ mgl v; mgr v ]
  let mgv v = [ mgt v; mgb v ]
  let pd = Css.padding.value
  let pdl = Css.padding.left
  let pdr = Css.padding.right
  let pdt = Css.padding.top
  let pdb = Css.padding.bottom
  let pdh v = [ pdl v; pdr v ]
  let pdv v = [ pdt v; pdb v ]
  let bd = Css.border.value
  let bdl = Css.border.left.value
  let bdr = Css.border.right.value
  let bdt = Css.border.top.value
  let bdb = Css.border.bottom.value
  let bdh v = [ bdl v; bdr v ]
  let bdv v = [ bdt v; bdb v ]
  let bdtl v = [ bdt v; bdl v ]
  let bdtr v = [ bdt v; bdr v ]
  let bdbl v = [ bdb v; bdl v ]
  let bdbr v = [ bdb v; bdr v ]
  let zin (z : string) = Css.zIndex z
  let opacity = Css.opacity
  let javascript (s : string) = script [ type' "text/javascript"; raw s ]
  let import url = script [ type' "text/javascript"; src url ]
  let stylesheet url = link [ href url; rel "stylesheet"; type' "text/css" ]
  let favicon url = link [ href url; rel "shortcut icon" ]
