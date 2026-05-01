//в качестве референса использовались "Faster, Simpler Red-Black Trees" и Data/Set/RBTree.hs
module RedBlackSet

type Color =
    | Red
    | Black

type Tree<'T> =
    | Empty
    | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

let emptySet = Empty

//оболочка, чтобы понимать, надо ли вызывать балансировку на следующих шагах рекурсии
type private Result<'T> =
    | Done of 'T
    | ToDo of 'T

//перекраска листа в черный
let private blacken tree =
    match tree with
    | Node(Red, a, x, b) -> Done(Node(Black, a, x, b))
    | _ -> ToDo tree

//убирает оболочку
let private justTree resultTree =
    match resultTree with
    | Done t -> t
    | ToDo t -> t

//считает черную высоту
let rec private blackHeight tree =
    match tree with
    | Empty -> 0
    | Node(Red, l, _, _) -> blackHeight l
    | Node(Black, l, _, _) -> 1 + (blackHeight l)


//проверка на наличие
let rec contains tree v =
    match tree with
    | Empty -> false
    | Node(_, left, value, right) ->
        if value > v then contains left v
        elif value < v then contains right v
        else true

//балансировка
let private balance tree =
    match tree with
    | Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
    | Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
    | Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
    | Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
        ToDo(Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d)))
    | Node(Black, a, x, b) as n -> Done(n)
    | _ -> ToDo(tree)

//вставка
let insert tree v =

    let rec insertRec tree v =
        match tree with
        | Empty -> ToDo(Node(Red, Empty, v, Empty))
        | Node(color, left, value, right) ->
            if value > v then
                let newLeft = insertRec left v

                match newLeft with
                | Done nl -> Done(Node(color, nl, value, right))
                | ToDo nl -> balance (Node(color, nl, value, right))
            elif value < v then
                let newRight = insertRec right v

                match newRight with
                | Done nr -> Done(Node(color, left, value, nr))
                | ToDo nr -> balance (Node(color, left, value, nr))
            else
                Done(tree)

    let newTree = insertRec tree v
    newTree |> justTree |> blacken |> justTree

//удаление
let delete tree v =

    let balanceDel tree =
        match tree with
        | Node(color, Node(Red, Node(Red, a, x, b), y, c), z, d)
        | Node(color, Node(Red, a, x, Node(Red, b, y, c)), z, d)
        | Node(color, a, x, Node(Red, Node(Red, b, y, c), z, d))
        | Node(color, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
            Done(Node(color, Node(Black, a, x, b), y, Node(Black, c, z, d)))
        | _ -> blacken tree

    let rec eqL tree =
        match tree with
        | Node(color, a, x, Node(Black, b, y, c)) -> balanceDel (Node(color, a, x, Node(Red, b, y, c)))
        | Node(color, a, x, Node(Red, b, y, c)) ->
            let newLeft = eqL (Node(Red, a, x, b))

            match newLeft with
            | Done nl -> Done(Node(Black, nl, y, c))
            | ToDo nl -> ToDo(Node(Black, nl, y, c))
        | _ -> failwith "Impossible pattern"

    let rec eqR tree =
        match tree with
        | Node(color, Node(Black, a, x, b), y, c) -> balanceDel (Node(color, Node(Red, a, x, b), y, c))
        | Node(color, Node(Red, a, x, b), y, c) ->
            let newRight = eqR (Node(Red, b, y, c))

            match newRight with
            | Done nr -> Done(Node(Black, a, x, nr))
            | ToDo nr -> ToDo(Node(Black, a, x, nr))
        | _ -> failwith "Impossible pattern"

    let delCur tree =

        let rec delMin tree =
            match tree with
            | Node(Red, Empty, x, b) -> (Done b, x)
            | Node(Black, Empty, x, b) -> (blacken b, x)
            | Node(color, a, x, b) ->
                let (an, min) = delMin a

                match an with
                | Done t -> (Done(Node(color, t, x, b)), min)
                | ToDo t -> (eqL (Node(color, t, x, b)), min)
            | _ -> failwith "Impossible pattern"

        match tree with
        | Node(Red, a, y, Empty) -> Done a
        | Node(Black, a, x, Empty) -> blacken a
        | Node(color, a, x, b) ->
            let (bn, min) = delMin b

            match bn with
            | Done t -> Done(Node(color, a, min, t))
            | ToDo t -> eqR (Node(color, a, min, t))
        | _ -> failwith "Impossible pattern"

    let rec deleteRec tree v =
        match tree with
        | Empty -> Done(Empty)
        | Node(color, left, value, right) ->
            if value > v then
                let newLeft = deleteRec left v

                match newLeft with
                | Done nl -> Done(Node(color, nl, value, right))
                | ToDo nl -> eqL (Node(color, nl, value, right))
            elif value < v then
                let newRight = deleteRec right v

                match newRight with
                | Done nr -> Done(Node(color, left, value, nr))
                | ToDo nr -> eqR (Node(color, left, value, nr))
            else
                delCur tree

    let newTree = deleteRec tree v
    newTree |> justTree |> blacken |> justTree

//join
let private join t1 g t2 =

    let rec joinLT t1 g t2 targetHeight currentHeight =
        if targetHeight = currentHeight then
            Node(Red, t1, g, t2)
        else
            match t2 with
            | Node(Red, l, x, r) ->
                let newLeft = joinLT t1 g l targetHeight currentHeight
                Node(Red, newLeft, x, r) |> balance |> justTree
            | Node(Black, l, x, r) ->
                let newLeft = joinLT t1 g l targetHeight (currentHeight - 1)
                Node(Black, newLeft, x, r) |> balance |> justTree
            | _ -> failwith "Impossible pattern"

    let rec joinRT t1 g t2 targetHeight currentHeight =
        if targetHeight = currentHeight then
            Node(Red, t1, g, t2)
        else
            match t1 with
            | Node(Red, l, x, r) ->
                let newRight = joinRT t2 g r targetHeight currentHeight
                Node(Red, l, x, newRight) |> balance |> justTree
            | Node(Black, l, x, r) ->
                let newRight = joinRT t2 g r targetHeight (currentHeight - 1)
                Node(Black, l, x, newRight) |> balance |> justTree
            | _ -> failwith "Impossible pattern"

    let h1 = blackHeight t1
    let h2 = blackHeight t2

    if h1 = 0 then
        insert t2 g
    else if h2 = 0 then
        insert t1 g
    else if h1 < h2 then
        let t = joinLT t1 g t2 h1 h2

        t |> blacken |> justTree
    else if h1 > h2 then
        let t = joinRT t1 g t2 h2 h1

        t |> blacken |> justTree
    else
        Node(Black, t1, g, t2)

//merge
let private merge t1 t2 =

    let rec minimum tree =
        match tree with
        | Node(_, Empty, x, _) -> x
        | Node(_, l, _, _) -> minimum l
        | _ -> failwith "Impossible pattern"

    let mergeEQ t1 t2 =
        let m = minimum t2
        let t2' = delete t2 m
        let h2' = blackHeight t2'
        let h1 = blackHeight t1

        if h1 = h2' then
            Node(Red, t1, m, t2')
        else
            match t1 with
            | Node(_, Node(Red, ll, lx, lr), x, r) -> Node(Red, Node(Black, ll, lx, lr), x, Node(Black, r, m, t2'))
            | Node(_, l, x, Node(Red, rl, rx, rr)) -> Node(Black, Node(Red, l, x, rl), rx, Node(Red, rr, m, t2'))
            | _ -> Node(Black, (justTree (blacken t1)), m, t2')

    let rec mergeLT t1 t2 targetHeight currentHeight =
        if targetHeight = currentHeight then
            mergeEQ t1 t2
        else
            match t2 with
            | Node(Red, l, x, r) ->
                let newLeft = mergeLT t1 l targetHeight currentHeight
                Node(Red, newLeft, x, r) |> balance |> justTree
            | Node(Black, l, x, r) ->
                let newLeft = mergeLT t1 l targetHeight (currentHeight - 1)
                Node(Red, newLeft, x, r) |> balance |> justTree
            | _ -> failwith "Impossible pattern"

    let rec mergeRT t1 t2 targetHeight currentHeight =
        if targetHeight = currentHeight then
            mergeEQ t1 t2
        else
            match t1 with
            | Node(Red, l, x, r) ->
                let newRight = mergeRT r t2 targetHeight currentHeight
                Node(Red, l, x, newRight) |> balance |> justTree
            | Node(Black, l, x, r) ->
                let newRight = mergeRT r t2 targetHeight (currentHeight - 1)
                Node(Red, l, x, newRight) |> balance |> justTree
            | _ -> failwith "Impossible pattern"

    let h1 = blackHeight t1
    let h2 = blackHeight t2

    if h1 = 0 then
        t2
    else if h2 = 0 then
        t1
    else if h1 < h2 then
        let t = mergeLT t1 t2 h1 h2

        t |> blacken |> justTree
    else if h1 > h2 then
        let t = mergeRT t1 t2 h2 h1

        t |> blacken |> justTree
    else
        let t = mergeEQ t1 t2
        t |> blacken |> justTree

//split
let rec private split kx tree =
    match tree with
    | Empty -> (Empty, Empty)
    | Node(_, l, x, r) ->
        if kx < x then
            let (lt, gt) = split kx l
            (lt, join gt x (justTree (blacken r)))
        else if kx > x then
            let (lt, gt) = split kx r
            (join (justTree (blacken l)) x lt, gt)
        else
            (justTree (blacken (l)), justTree (blacken (r)))


//объединение
let rec union t1 t2 =
    match t1 with
    | Empty -> t2
    | _ ->
        match t2 with
        | Empty -> t1
        | Node(_, l, x, r) ->
            let (l', r') = split x t1
            join (union l' l) x (union r' r)

//пересечение
let rec intersection t1 t2 =
    match t1 with
    | Empty -> Empty
    | _ ->
        match t2 with
        | Empty -> Empty
        | Node(_, l, x, r) ->
            let (l', r') = split x t1

            if contains t1 x then
                join (intersection l' l) x (intersection r' r)
            else
                merge (intersection l' l) (intersection r' r)

//разность
let rec difference t1 t2 =
    match t1 with
    | Empty -> Empty
    | _ ->
        match t2 with
        | Empty -> t1
        | Node(_, l, x, r) ->
            let (l', r') = split x t1
            merge (difference l' l) (difference r' r)
