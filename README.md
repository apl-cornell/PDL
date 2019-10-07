# SpecLang

This repository contains notes and examples for a mid-high level hardware description language,
which abstracts communication between computation stages for the purpose
of proving time-dependent properties. In particular, one idea is to
prove that speculative state is always "verified" before reaching
architectural state (i.e. state which may not be speculative).

The `notes` directory contains various ideas for languages that fulfill this goal.
In particular it currently contains a `semantics` directory that contains the specification for
potential semantics for a "speculation aware" language, inspired by "type state".

The `examples` directory contains example programs and explanations as
to what they should or should not do.