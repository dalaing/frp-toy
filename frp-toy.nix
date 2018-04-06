{ mkDerivation, base, comonad, containers, contravariant
, dependent-map, dependent-sum, distributive, free, lens, mtl
, prim-uniq, primitive, stdenv, stm, transformers
}:
mkDerivation {
  pname = "frp-toy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base comonad containers contravariant dependent-map dependent-sum
    distributive free lens mtl prim-uniq primitive stm transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
