module Map

type TreeMap<'K, 'V> =
    | Empty
    | Node of key: 'K * value: 'V * height: int * left: TreeMap<'K, 'V> * right: TreeMap<'K, 'V>

let private height tree =
    match tree with
    | Empty -> 0
    | Node(_, _, h, _, _) -> h

let private makeNode k v l r =
    let h = 1 + max (height l) (height r)
    Node(k, v, h, l, r)        

let private rotateRight tree= 
    match tree with
    | Empty -> Empty
    | Node(x, vx, _, left, right) ->
        match left with
        | Empty -> failwith "rotateRight: left child is empty"
        | Node(y, vy, _, ly, ry) ->
            Node(y, vy,
                 1 + max (height ly) (height (makeNode x vx ry right)),
                 ly, makeNode x vx ry right)

let private rotateLeft tree= 
    match tree with
    | Empty -> Empty
    | Node(x, vx, _, left, right) ->
        match right with
        | Empty -> failwith "rotateLeft: right child is empty"
        | Node(y, vy, _, ly, ry) ->
            Node(y, vy,
                 1 + max (height (makeNode x vx left ly)) (height ry),
                 makeNode x vx left ly, ry)

let private balance k v l r =
    let hl = height l
    let hr = height r
    if hl - hr > 1 then        
        match l with
        | Node(lk, lv, _, ll, lr) when height ll >= height lr ->            
            rotateRight (Node(k, v, 0, l, r))
        | Node(lk, lv, _, ll, lr) ->
            let newLeft = rotateLeft (Node(lk, lv, 0, ll, lr))
            rotateRight (Node(k, v, 0, newLeft, r))
        | _ -> failwith "balance: left heavy but left is empty"
    elif hr - hl > 1 then
        match r with
        | Node(rk, rv, _, rl, rr) when height rr >= height rl ->
            rotateLeft (Node(k, v, 0, l, r))
        | Node(rk, rv, _, rl, rr) ->
            let newRight = rotateRight (Node(rk, rv, 0, rl, rr))
            rotateLeft (Node(k, v, 0, l, newRight))
        | _ -> failwith "balance: right heavy but right is empty"
    else
        makeNode k v l r

let empty = Empty

let isEmpty map =
    match map with Empty -> true | _ -> false

let rec contains k map = 
    match map with
    | Empty -> false
    | Node(k2, _, _, l, r) ->
        if k = k2 then true
        elif k < k2 then contains k l
        else contains k r

let rec tryFind k map = 
    match map with
    | Empty -> None
    | Node(k2, v, _, l, r) ->
        if k = k2 then Some v
        elif k < k2 then tryFind k l
        else tryFind k r


let rec add k v map =
    match map with
    | Empty -> Node(k, v, 1, Empty, Empty)
    | Node(k2, v2, _, l, r) ->
        if k < k2 then
            let newL = add k v l
            balance k2 v2 newL r
        elif k > k2 then
            let newR = add k v r
            balance k2 v2 l newR
        else
            // If key already exists --- update value
            Node(k, v, height (Node(k2, v2, 0, l, r)), l, r)


/// Removes the entry for the given key. If the key is not exists, the map is unchanged.
let rec remove k map =
    let rec minKeyValue map =
        match map with
        | Empty -> failwith "minKeyValue: empty map"
        | Node(k, v, _, Empty, _) -> (k, v)
        | Node(_, _, _, l, _) -> minKeyValue l

    let rec removeMin map =
        match map with
        | Empty -> Empty
        | Node(k, v, _, Empty, r) -> r
        | Node(k, v, _, l, r) -> balance k v (removeMin l) r

    match map with
    | Empty -> Empty
    | Node(k2, v2, _, l, r) ->
        if k < k2 then
            balance k2 v2 (remove k l) r
        elif k > k2 then
            balance k2 v2 l (remove k r)
        else            
            match l, r with
            | Empty, _ -> r
            | _, Empty -> l
            | _ ->
                let (minK, minV) = minKeyValue r
                let newR = removeMin r
                balance minK minV l newR

/// Folds over the map in ascending key order.
let rec fold f acc = function
    | Empty -> acc
    | Node(k, v, _, l, r) ->
        let acc' = fold f acc l
        let acc'' = f acc' k v
        fold f acc'' r


/// Returns the number of entries in the map.
let rec count map = fold (fun acc _ _ -> acc + 1) 0 map

/// Returns a list of (key, value) pairs in ascending key order.
let toList map = fold (fun acc k v -> (k, v) :: acc) [] map |> List.rev

/// Creates a map from a sequence of key‑value pairs.
let ofList list =
    List.fold (fun acc (k, v) -> add k v acc) empty list
