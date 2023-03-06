---
title: "On Security Against Time Traveling Adversaries"
date: 2022-09-04
type: paper
author:
  - "Lúcás C. Meier"
withpost: false
description: ""
link: "https://eprint.iacr.org/2022/1148"
---

If you had a time machine, what cryptography would you be able to break?

In this work, we investigate the notion of time travel, formally defining models for adversaries equipped with a time machine, and exploring the consequences for cryptography. We find that being able to rewind time breaks some cryptographic schemes, and being able to freely move both forwards and backwards in time breaks even more schemes.

We look at the impacts of time travel on encryption and signatures in particular, finding that the **IND-CCA** and **EUF-CMA** security games are broken, while **IND-CPA** and **UUF-CMA** remain secure.