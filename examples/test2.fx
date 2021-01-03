def f(x) =
  let a =
    let b =
       if x > 0 then
         f (x-1)
       else
         1
    in b + 2
  in a + 3
val _ = print_int(f(3))
val _ = print_string("\n")
/* resultat : 21 */
