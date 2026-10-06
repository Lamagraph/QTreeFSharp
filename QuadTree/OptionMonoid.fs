module OptionMonoid

let inline op_add x y =
    match x, y with
    | Some a, Some b -> Some(a + b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

let inline op_mult x y =
    match x, y with
    | Some a, Some b -> Some(a * b)
    | _ -> None
