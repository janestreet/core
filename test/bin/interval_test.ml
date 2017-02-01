open OUnit;;
open Core

let test =
  "interval" >:::
  [ "is_empty_or_singleton" >::
    (fun () ->
       let t x = Interval.is_empty_or_singleton x in
       let i = Interval.create in
       "singleton1" @? t (i 0 0);
       "singleton2" @? t (i 10 10);
       "singleton3" @? t (i "foo" "foo");
       "empty1" @? t (i 1 0);
       "nonempty" @? not (t (i 0 1));
    );

    "are_disjoint_as_open_intervals" >::
    (fun () ->
       let t x = Interval.are_disjoint_as_open_intervals x in
       let i = Interval.create in
       "touching" @? t [i 3 4; i 4 5];
       "not touching" @? t [i 3 4; i 5 6];
       "overlapping" @? not (t [i 3 5; i 4 6]);
    );

    "contains_set" >::
    (fun () ->
       let module S = Interval.Set in
       let s1 = S.create [ 1,2; 3,4; 5,6 ] in
       let s2 = S.create [ 3,5; 10,11 ] in
       let s3 = S.create [ 3,4 ] in
       "contains 1" @? (S.contains s2 3);
       "contains 2" @? (S.contains s2 4);
       "contains 3" @? (not (S.contains s2 9));
       "contains 4" @? (not (S.contains s2 12));
       "contains_set 1" @? (not (S.contains_set ~container:s2 ~contained:s1));
       "contains_set 2" @? (not (S.contains_set ~container:s1 ~contained:s2));
       "contains_set 3" @? (S.contains_set ~container:s1 ~contained:s3);
       "contains_set 4" @? (S.contains_set ~container:s2 ~contained:s3);
    );

    "half_open_intervals_are_a_partition" >::
    (fun () ->
       "are_a_partition" @? Interval.half_open_intervals_are_a_partition [
         Interval.create 0 2;
         Interval.create 2 4;
         Interval.create 4 8;
       ];
       "not_a_partition" @? not (Interval.half_open_intervals_are_a_partition [
         Interval.create 0 2;
         Interval.create 2 4;
         Interval.create 5 8;
       ]);
    );
  ]

