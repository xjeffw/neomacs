# Neomacs PR Checklist

## Compatibility Checks

- [ ] Ran relevant vm-compat checks for changed areas (for example `make -C test/neovm/vm-compat check-one-neovm`, `check-all-neovm`, etc.).
- [ ] Ran `make -C test/neovm/vm-compat compat-progress` and attached output in PR notes.
- [ ] If stub annotations changed, ran `make -C test/neovm/vm-compat check-stub-budget` and confirmed `explicitly annotated function stubs` budget expectations.
- [ ] If case corpus changed, attached `make -C test/neovm/vm-compat check` or `check-neovm` outputs for changed case lists.
- [ ] If registry metadata changed, attached `check-builtin-registry-*` output (at least `check-builtin-registry-fboundp`).
