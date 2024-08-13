;; imported from https://github.com/ocaml-wasm/wasm_of_ocaml/issues/5
(module
   (import "env" "lazy_tag" (global $lazy_tag i32))
   (import "env" "forward_tag" (global $forward_tag i32))

   (type $block (array (mut (ref eq))))

   (func (export "core_heap_block_is_heap_block")
      (param (ref eq)) (result (ref eq))
      (local $tag i32)
      (drop (block $not_block (result (ref eq))
         (local.set $tag
            (i31.get_u
               (ref.cast (ref i31)
               (array.get $block
                  (br_on_cast_fail $not_block (ref eq) (ref $block)
                     (local.get 0))
                  (i32.const 0)))))
         (return
            (ref.i31
               (i32.eqz
                  (i32.or
                     (i32.eq (local.get $tag) (global.get $lazy_tag))
                     (i32.eq (local.get $tag) (global.get $forward_tag))))))))
      (ref.i31 (i32.const 0)))
)
