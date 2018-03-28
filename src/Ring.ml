(* @todo Move this to General *)

open General.Abbr
module OCSA = OCamlStandard.ArrayLabels

(* Auto-rotating, bounded-size array. Index zero always holds the most recently added element.
Old elements are discarded when max_size elements have been added after them.
Indexes greater than min(max_size, number of elements added)-1 are out of bound. *)

type 'a t = {
  max_size: int;
  items: 'a array;
  head: int;
}

let make max_size =
  assert (max_size > 0);
  {max_size; items=[||]; head=0}

let max_size {max_size; _} =
  max_size

let size {items; _} =
  OCSA.length items

let add {max_size; items; head} ~v =
  let size = OCSA.length items in
  if size < max_size then begin
    let new_items = OCSA.make (size + 1) v in
    OCSA.blit ~src:items ~src_pos:0 ~dst:new_items ~dst_pos:1 ~len:size;
    {max_size; items=new_items; head=0}
  end else begin
    assert (size = max_size);
    let items = OCSA.copy items in
    let head = (max_size + head - 1) mod max_size in
    items.(head) <- v;
    {max_size; items; head}
  end

let get {max_size; items; head} ~index =
  let size = OCSA.length items in
  if index >= Int.min max_size size then Exn.invalid_argument "index out of bounds";
  let index = (index + head) mod max_size in
  OCSA.unsafe_get items index

module Test = struct
  open Tst

  let out_of_bounds = Exn.InvalidArgument("index out of bounds")

  let test = "Ring" >:: [
    "size 2" >:: [
      "0" >:: (
        let xs = make 2 in
        [
          "max_size" >: (lazy (check_int ~expected:2 (max_size xs)));
          "size" >: (lazy (check_int ~expected:0 (size xs)));
          "get 0" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:0))));
          "get 1" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:1))));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
        ]
      );
      "1" >:: (
        let xs = make 2 |> add ~v:42 in
        [
          "max_size" >: (lazy (check_int ~expected:2 (max_size xs)));
          "size" >: (lazy (check_int ~expected:1 (size xs)));
          "get 0" >: (lazy (check_int ~expected:42 (get xs ~index:0)));
          "get 1" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:1))));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
        ]
      );
      "2" >:: (
        let xs = make 2 |> add ~v:42 |> add ~v:43 in
        [
          "max_size" >: (lazy (check_int ~expected:2 (max_size xs)));
          "size" >: (lazy (check_int ~expected:2 (size xs)));
          "get 0" >: (lazy (check_int ~expected:43 (get xs ~index:0)));
          "get 1" >: (lazy (check_int ~expected:42 (get xs ~index:1)));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
        ]
      );
      "3" >:: (
        let xs = make 2 |> add ~v:42 |> add ~v:43 |> add ~v:44 in
        [
          "max_size" >: (lazy (check_int ~expected:2 (max_size xs)));
          "size" >: (lazy (check_int ~expected:2 (size xs)));
          "get 0" >: (lazy (check_int ~expected:44 (get xs ~index:0)));
          "get 1" >: (lazy (check_int ~expected:43 (get xs ~index:1)));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
        ]
      );
      "4" >:: (
        let xs = make 2 |> add ~v:42 |> add ~v:43 |> add ~v:44 |> add ~v:45 in
        [
          "max_size" >: (lazy (check_int ~expected:2 (max_size xs)));
          "size" >: (lazy (check_int ~expected:2 (size xs)));
          "get 0" >: (lazy (check_int ~expected:45 (get xs ~index:0)));
          "get 1" >: (lazy (check_int ~expected:44 (get xs ~index:1)));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
        ]
      );
    ];
    "size 5" >:: [
      "1" >:: (
        let xs = make 5 |> add ~v:42 in
        [
          "max_size" >: (lazy (check_int ~expected:5 (max_size xs)));
          "size" >: (lazy (check_int ~expected:1 (size xs)));
          "get 0" >: (lazy (check_int ~expected:42 (get xs ~index:0)));
          "get 1" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:1))));
          "get 2" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:2))));
          "get 3" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:3))));
          "get 4" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:4))));
          "get 5" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:5))));
          "get 6" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:6))));
        ]
      );
      "4" >:: (
        let xs = make 5 |> add ~v:42 |> add ~v:43 |> add ~v:44 |> add ~v:45 in
        [
          "max_size" >: (lazy (check_int ~expected:5 (max_size xs)));
          "size" >: (lazy (check_int ~expected:4 (size xs)));
          "get 0" >: (lazy (check_int ~expected:45 (get xs ~index:0)));
          "get 0" >: (lazy (check_int ~expected:44 (get xs ~index:1)));
          "get 0" >: (lazy (check_int ~expected:43 (get xs ~index:2)));
          "get 0" >: (lazy (check_int ~expected:42 (get xs ~index:3)));
          "get 4" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:4))));
          "get 5" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:5))));
          "get 6" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:6))));
        ]
      );
      "5" >:: (
        let xs = make 5 |> add ~v:42 |> add ~v:43 |> add ~v:44 |> add ~v:45 |> add ~v:46 in
        [
          "max_size" >: (lazy (check_int ~expected:5 (max_size xs)));
          "size" >: (lazy (check_int ~expected:5 (size xs)));
          "get 0" >: (lazy (check_int ~expected:46 (get xs ~index:0)));
          "get 0" >: (lazy (check_int ~expected:45 (get xs ~index:1)));
          "get 0" >: (lazy (check_int ~expected:44 (get xs ~index:2)));
          "get 0" >: (lazy (check_int ~expected:43 (get xs ~index:3)));
          "get 0" >: (lazy (check_int ~expected:42 (get xs ~index:4)));
          "get 5" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:5))));
          "get 6" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:6))));
        ]
      );
      "6" >:: (
        let xs = make 5 |> add ~v:42 |> add ~v:43 |> add ~v:44 |> add ~v:45 |> add ~v:46 |> add ~v:47 in
        [
          "max_size" >: (lazy (check_int ~expected:5 (max_size xs)));
          "size" >: (lazy (check_int ~expected:5 (size xs)));
          "get 0" >: (lazy (check_int ~expected:47 (get xs ~index:0)));
          "get 0" >: (lazy (check_int ~expected:46 (get xs ~index:1)));
          "get 0" >: (lazy (check_int ~expected:45 (get xs ~index:2)));
          "get 0" >: (lazy (check_int ~expected:44 (get xs ~index:3)));
          "get 0" >: (lazy (check_int ~expected:43 (get xs ~index:4)));
          "get 5" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:5))));
          "get 6" >: (lazy (expect_exception ~expected:out_of_bounds (lazy (get xs ~index:6))));
        ]
      );
    ];
  ]
end
