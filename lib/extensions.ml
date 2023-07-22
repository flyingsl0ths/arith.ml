module String = struct
  include String

  let tl cs = String.sub cs 1 @@ (String.length cs - 1)
  let hd = function "" -> ' ' | cs -> String.get cs 0
  let null = function "" -> true | _ -> false

  let span f = function
    | "" -> ("", "")
    | cs ->
        let rec span' acc cs' =
          let hd' = hd cs' in
          match cs' with
          | "" -> acc
          | cs'' when f hd' ->
              let rest = tl cs'' in
              span' (fst acc ^ Char.escaped @@ hd', rest) rest
          | _ -> acc
        in
        span' ("", "") cs

  let dropWhile f = function
    | "" -> ""
    | cs ->
        let rec dropWhile' cs' =
          let hd' = hd cs' in
          match cs' with
          | "" -> cs'
          | cs'' when f hd' ->
              let rest = tl cs'' in
              dropWhile' rest
          | _ -> cs'
        in
        dropWhile' cs
end

module Char = struct
  include Char

  let is_digit c = c >= '0' && c <= '9'
end
