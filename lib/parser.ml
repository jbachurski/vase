module Grammar = struct
  open Lexer

  open Parc.Make (struct
    type c = token
  end)

  let name = next (function Name _ -> true | _ -> false)
end
