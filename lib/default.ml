type ('real, 'phantom) t = 'real

let override = Fn.id

module type S = sig
  type phantom
  type real
  type default = (real, phantom) t
  val default : default
end

let create (type real_) (default : real_) =
  (module struct
    type phantom
    type real = real_
    type default = (real, phantom) t
    let default = default
  end : S with type real = real_)
;;

