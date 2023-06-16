module type Functor_base = sig
  type 'a f

  val ( <$> ) : ('a -> 'b) -> 'a f -> 'b f
end

module type Applicative_base = sig
  type 'a f

  val pure : 'a -> 'a f
  val ( <*> ) : ('a -> 'b) f -> 'a f -> 'b f
end

module type Monad_base = sig
  type 'a m

  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
end

module Functor (F : Functor_base) = struct
  include F

  let ( <$ ) a x = (fun _ -> a) <$> x
  let ( $> ) x a = a <$ x
end

module Applicative (F : Functor_base) (A : Applicative_base with type 'a f = 'a F.f) =
struct
  include Functor (F)
  include A

  let liftA2 f x y = f <$> x <*> y
  let ( <**> ) a f = f <*> a
  let ( *> ) a b = liftA2 (fun _ x -> x) a b
  let ( <* ) a b = liftA2 (fun x _ -> x) a b
end

module Monad
    (F : Functor_base)
    (A : Applicative_base with type 'a f = 'a F.f)
    (M : Monad_base with type 'a m = 'a F.f) =
struct
  include Functor (F)
  include Applicative (F) (A)
  include M

  let ( >> ) m k = m >>= fun _ -> k
end
