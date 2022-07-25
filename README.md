* No Optimisation


```
ghc -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -fforce-recomp forDdumpComp.hs

./forDdumpComp +RTS -s
```

```
ghc -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -fforce-recomp forDdumpCompNew.hs

./forDdumpCompNew +RTS -s
```

Both perform badly

You can look at the `.dump-simpl` files and they are different.

* -O1 and -O2

```
ghc -O1 -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -fforce-recomp forDdumpComp.hs

./forDdumpComp +RTS -s
```

```
ghc -O1 -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-idinfo -fforce-recomp forDdumpCompNew.hs

./forDdumpCompNew +RTS -s
```

Now both perform better but the specialised version is about x5 faster!
