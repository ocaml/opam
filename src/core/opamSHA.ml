(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat

module SHA256 = struct

  open Int32

  let k = [|
    0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
    0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
    0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
    0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
    0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
    0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
    0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
    0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
    0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
  |]

  let ch x y z =
    logxor (logand x y) (logand (lognot x) z)

  let maj x y z =
    logxor (logand x y) (logxor (logand x z) (logand y z))

  let sum0 x =
    logxor
      (logor (shift_right_logical x 2) (shift_left x (32 - 2)))
      (logxor (logor (shift_right_logical x 13) (shift_left x (32 - 13)))
         (logor (shift_right_logical x 22) (shift_left x (32 - 22))))

  let sum1 x =
    logxor
      (logor (shift_right_logical x 6) (shift_left x (32 - 6)))
      (logxor (logor (shift_right_logical x 11) (shift_left x (32 - 11)))
         (logor (shift_right_logical x 25) (shift_left x (32 - 25))))

  let lsig0 x =
    logxor
      (logor (shift_right_logical x 7) (shift_left x (32 - 7)))
      (logxor (logor (shift_right_logical x 18) (shift_left x (32 - 18)))
         (logor (shift_right_logical x 3) (shift_right_logical x 3)))

  let lsig1 x =
    logxor
      (logor (shift_right_logical x 17) (shift_left x (32 - 17)))
      (logxor (logor (shift_right_logical x 19) (shift_left x (32 - 19)))
         (logor (shift_right_logical x 10) (shift_right_logical x 10)))

  let sha_init = (
    0x6a09e667l, 0xbb67ae85l, 0x3c6ef372l, 0xa54ff53al,
    0x510e527fl, 0x9b05688cl, 0x1f83d9abl, 0x5be0cd19l
  )

  external swap32 : int32 -> int32 = "%bswap_int32"
  external unsafe_get_32 : Bytes.t -> int -> int32 = "%caml_string_get32u"

  let toggle_big_endian i32 =
    if Sys.big_endian then i32
    else swap32 i32

  let hash_block =
    let warr = Array.make 64 0l in
    fun hh block ->
      for t = 0 to 15 do
        warr.(t) <- toggle_big_endian (Bigarray.Array1.get block t)
      done;
      for t = 16 to 63 do
        warr.(t) <-
          add
            (add (lsig1 warr.(t - 2)) warr.(t - 7))
            (add (lsig0 warr.(t - 15)) warr.(t - 16))
      done;

      let rec stir t (a, b, c, d, e, f, g, h) =
        if t >= 64 then
          let a', b', c', d', e', f', g', h' = hh in
          add a a', add b b', add c c', add d d',
          add e e', add f f', add g g', add h h'
        else
        let t1 =
          add (add h (sum1 e)) (add (add (ch e f g) k.(t)) warr.(t))
        in
        let t2 = add (sum0 a) (maj a b c) in
        stir (t + 1)
          (add t1 t2, a, b, c, add d t1, e, f, g)
      in
      stir 0 hh

  let hash f =
    let fd = Unix.openfile f [Unix.O_RDONLY] 0 in
    let sz = (Unix.fstat fd).Unix.st_size in
    let blocks = sz / 64 in
    let rem = sz mod 64 in
    let a = Bigarray.(Array2.map_file fd int32 c_layout false blocks 16) in
    let h = ref sha_init in
    for i = 0 to blocks - 1 do
      h := hash_block !h (Bigarray.Array2.slice_left a i)
    done;
    let lastblock = Bigarray.(Array1.create int32 c_layout 16) in
    let buf = Bytes.create rem in
    ignore (Unix.lseek fd (blocks * 64) Unix.SEEK_SET);
    let rec readn i =
      let r = Unix.read fd buf i (rem - i) in
      if r < rem - i then readn (i + r)
    in
    readn 0;
    Unix.close fd;
    for i = 0 to rem / 4 - 1 do
      Bigarray.Array1.set lastblock i (unsafe_get_32 buf (i*4))
    done;
    Bigarray.Array1.set lastblock (rem / 4) 0l;
    let c k = of_int (int_of_char (Bytes.get buf k)) in
    let setbyte block i c =
      let shift =
        if Sys.big_endian then 8 * (3 - i mod 4)
        else 8 * (i mod 4)
      in
      Bigarray.Array1.set block (i / 4)
        (logor (Bigarray.Array1.get block (i / 4)) (shift_left c shift))
    in
    for k = rem - rem mod 4 to rem - 1 do
      setbyte lastblock k (c k)
    done;
    if rem <= 55 then (
      setbyte lastblock rem 0x80l;
      for i = rem + 1 to rem + 3 - rem mod 4 do
        setbyte lastblock i 0l
      done;
      for i = rem / 4 + 1 to 13 do
        Bigarray.Array1.set lastblock i 0l
      done
    ) else (
      for i = rem / 4 + 1 to 15 do
        Bigarray.Array1.set lastblock i 0l
      done;
      setbyte lastblock rem 0x80l;
      h := hash_block !h lastblock;
      (* reuse lastblock as the extra size block *)
      for i = 0 to 13 do
        Bigarray.Array1.set lastblock i 0l
      done
    );
    let bitsz = Int64.mul 8L (Int64.of_int sz) in
    Bigarray.Array1.set lastblock 14
      (toggle_big_endian Int64.(to_int32 (shift_right_logical bitsz 32)));
    Bigarray.Array1.set lastblock 15
      (toggle_big_endian Int64.(to_int32 (logand 0xffffffffL bitsz)));
    let (a, b, c, d, e, f, g, h) =  hash_block !h lastblock in
    Printf.sprintf "%08lx%08lx%08lx%08lx%08lx%08lx%08lx%08lx" a b c d e f g h
end

module SHA512 = struct

  open Int64

  let k = [|
    0x428a2f98d728ae22L; 0x7137449123ef65cdL;
    0xb5c0fbcfec4d3b2fL; 0xe9b5dba58189dbbcL;
    0x3956c25bf348b538L; 0x59f111f1b605d019L;
    0x923f82a4af194f9bL; 0xab1c5ed5da6d8118L;
    0xd807aa98a3030242L; 0x12835b0145706fbeL;
    0x243185be4ee4b28cL; 0x550c7dc3d5ffb4e2L;
    0x72be5d74f27b896fL; 0x80deb1fe3b1696b1L;
    0x9bdc06a725c71235L; 0xc19bf174cf692694L;
    0xe49b69c19ef14ad2L; 0xefbe4786384f25e3L;
    0x0fc19dc68b8cd5b5L; 0x240ca1cc77ac9c65L;
    0x2de92c6f592b0275L; 0x4a7484aa6ea6e483L;
    0x5cb0a9dcbd41fbd4L; 0x76f988da831153b5L;
    0x983e5152ee66dfabL; 0xa831c66d2db43210L;
    0xb00327c898fb213fL; 0xbf597fc7beef0ee4L;
    0xc6e00bf33da88fc2L; 0xd5a79147930aa725L;
    0x06ca6351e003826fL; 0x142929670a0e6e70L;
    0x27b70a8546d22ffcL; 0x2e1b21385c26c926L;
    0x4d2c6dfc5ac42aedL; 0x53380d139d95b3dfL;
    0x650a73548baf63deL; 0x766a0abb3c77b2a8L;
    0x81c2c92e47edaee6L; 0x92722c851482353bL;
    0xa2bfe8a14cf10364L; 0xa81a664bbc423001L;
    0xc24b8b70d0f89791L; 0xc76c51a30654be30L;
    0xd192e819d6ef5218L; 0xd69906245565a910L;
    0xf40e35855771202aL; 0x106aa07032bbd1b8L;
    0x19a4c116b8d2d0c8L; 0x1e376c085141ab53L;
    0x2748774cdf8eeb99L; 0x34b0bcb5e19b48a8L;
    0x391c0cb3c5c95a63L; 0x4ed8aa4ae3418acbL;
    0x5b9cca4f7763e373L; 0x682e6ff3d6b2b8a3L;
    0x748f82ee5defb2fcL; 0x78a5636f43172f60L;
    0x84c87814a1f0ab72L; 0x8cc702081a6439ecL;
    0x90befffa23631e28L; 0xa4506cebde82bde9L;
    0xbef9a3f7b2c67915L; 0xc67178f2e372532bL;
    0xca273eceea26619cL; 0xd186b8c721c0c207L;
    0xeada7dd6cde0eb1eL; 0xf57d4f7fee6ed178L;
    0x06f067aa72176fbaL; 0x0a637dc5a2c898a6L;
    0x113f9804bef90daeL; 0x1b710b35131c471bL;
    0x28db77f523047d84L; 0x32caab7b40c72493L;
    0x3c9ebe0a15c9bebcL; 0x431d67c49c100d4cL;
    0x4cc5d4becb3e42b6L; 0x597f299cfc657e2aL;
    0x5fcb6fab3ad6faecL; 0x6c44198c4a475817L;
  |]

  let rotate x n = logor (shift_right_logical x n) (shift_left x (64 - n))

  let ch x y z = logxor (logand x y) (logand (lognot x) z)

  let maj x y z = logxor (logand x y) (logxor (logand x z) (logand y z))

  let sum0 x = logxor (rotate x 28) (logxor (rotate x 34) (rotate x 39))

  let sum1 x = logxor (rotate x 14) (logxor (rotate x 18) (rotate x 41))

  let lsig0 x =
    logxor (rotate x 1) (logxor (rotate x 8) (shift_right_logical x 7))

  let lsig1 x =
    logxor (rotate x 19) (logxor (rotate x 61) (shift_right_logical x 6))

  let sha_init = (
    0x6a09e667f3bcc908L, 0xbb67ae8584caa73bL,
    0x3c6ef372fe94f82bL, 0xa54ff53a5f1d36f1L,
    0x510e527fade682d1L, 0x9b05688c2b3e6c1fL,
    0x1f83d9abfb41bd6bL, 0x5be0cd19137e2179L
  )

  external swap64 : int64 -> int64 = "%bswap_int64"
  external unsafe_get_64 : Bytes.t -> int -> int64 = "%caml_string_get64u"

  let toggle_big_endian i64 =
    if Sys.big_endian then i64
    else swap64 i64

  let hash_block =
    let warr = Array.make 80 0L in
    fun hh block ->
      for t = 0 to 15 do
        warr.(t) <- toggle_big_endian (Bigarray.Array1.get block t)
      done;
      for t = 16 to 79 do
        warr.(t) <-
          add
            (add (lsig1 warr.(t - 2)) warr.(t - 7))
            (add (lsig0 warr.(t - 15)) warr.(t - 16))
      done;

      let rec stir t (a, b, c, d, e, f, g, h) =
        if t >= 80 then
          let a', b', c', d', e', f', g', h' = hh in
          add a a', add b b', add c c', add d d',
          add e e', add f f', add g g', add h h'
        else
        let t1 =
          add (add h (sum1 e)) (add (add (ch e f g) k.(t)) warr.(t))
        in
        let t2 = add (sum0 a) (maj a b c) in
        stir (t + 1)
          (add t1 t2, a, b, c, add d t1, e, f, g)
      in
      stir 0 hh

  let hash f =
    let fd = Unix.openfile f [Unix.O_RDONLY] 0 in
    let sz = (Unix.fstat fd).Unix.st_size in
    let blocks = sz / 128 in
    let rem = sz mod 128 in
    let a = Bigarray.(Array2.map_file fd int64 c_layout false blocks 16) in
    let h = ref sha_init in
    for i = 0 to blocks - 1 do
      h := hash_block !h (Bigarray.Array2.slice_left a i)
    done;
    let lastblock = Bigarray.(Array1.create int64 c_layout 16) in
    let buf = Bytes.create rem in
    ignore (Unix.lseek fd (blocks * 128) Unix.SEEK_SET);
    let rec readn i =
      let r = Unix.read fd buf i (rem - i) in
      if r < rem - i then readn (i + r)
    in
    readn 0;
    Unix.close fd;
    for i = 0 to rem / 8 - 1 do
      Bigarray.Array1.set lastblock i (unsafe_get_64 buf (i*8))
    done;
    Bigarray.Array1.set lastblock (rem / 8) 0L;
    let c k = of_int (int_of_char (Bytes.get buf k)) in
    let setbyte block i c =
      let shift =
        if Sys.big_endian then 8 * (7 - i mod 8)
        else 8 * (i mod 8)
      in
      Bigarray.Array1.set block (i / 8)
        (logor (Bigarray.Array1.get block (i / 8)) (shift_left c shift))
    in
    for k = rem - rem mod 8 to rem - 1 do
      setbyte lastblock k (c k)
    done;
    if rem <= 111 then (
      setbyte lastblock rem 0x80L;
      for i = rem + 1 to rem + 7 - rem mod 8 do
        setbyte lastblock i 0L
      done;
      for i = rem / 8 + 1 to 13 do
        Bigarray.Array1.set lastblock i 0L
      done
    ) else (
      for i = rem / 8 + 1 to 15 do
        Bigarray.Array1.set lastblock i 0L
      done;
      setbyte lastblock rem 0x80L;
      h := hash_block !h lastblock;
      (* reuse lastblock as the extra size block *)
      for i = 0 to 13 do
        Bigarray.Array1.set lastblock i 0L
      done
    );
    (* We assume sz fits in 61 bits... *)
    let bitsz = Int64.mul 8L (Int64.of_int sz) in
    Bigarray.Array1.set lastblock 14 0L;
    Bigarray.Array1.set lastblock 15 (toggle_big_endian bitsz);
    let (a, b, c, d, e, f, g, h) = hash_block !h lastblock in
    Printf.sprintf "%016Lx%016Lx%016Lx%016Lx%016Lx%016Lx%016Lx%016Lx"
      a b c d e f g h

end

let sha256 = SHA256.hash
let sha512 = SHA512.hash
let hash = function
  | `SHA256 -> SHA256.hash
  | `SHA512 -> SHA512.hash
