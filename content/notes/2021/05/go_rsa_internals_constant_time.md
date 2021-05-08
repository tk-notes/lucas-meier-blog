---
title: "Making Go's RSA Internals Constant Time"
date: 2021-05-07T21:41:01+02:00
type: note
note-tags:
  - "Math"
  - "Cryptography"
katex: true
---

This is essentially a lab notebook, while trying to move Go's
`crypto/rsa` module away from the variable-time `math/big.Int`, to an
internal constant-time number type.

# 2020-05-07

## Necessary Methods

Key generation is out of the scope for now, and substantially more
complicated compared to encryption and decryption.
For implementing these, we only need:

- `CmpGeq`
- `CmpZero`
- `ModInv` *(Only if we use blinding)*
- `ModExp`
- `ModMul`
- `ModAdd`
- `ModSub`

If we implement constant-time operations, we can get rid of blinding,
and this provides additional incentive, since we don't need
to implement modular inversion, and can remove the blinding logic.

## Internal APIs

Internal APIs often use `big.Int`, and could be shifted to use
our internal type. The public API still needs to use `big.Int` though,
for compatability.

This would help with some leakages actually, because using `big.Int`
leaks zero padding information.

The concern here is the complexity of making this change.
