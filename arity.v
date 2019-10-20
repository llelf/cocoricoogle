
Ltac arity x :=
  let ty := type of x in
  let arity := match eval hnf in ty with
  | forall a b c d e f, _ => 6
  | forall a b c d e, _   => 5
  | forall a b c d, _     => 4
  | forall a b c, _       => 3
  | forall a b, _         => 2
  | forall a, _           => 1
  |  _ => 0
  end in idtac "arity =" arity ".".

