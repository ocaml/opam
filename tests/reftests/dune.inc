
(rule
 (alias reftest-assume-built)
 (action
  (diff assume-built.test assume-built.out)))

(alias
 (name reftest)
 (deps (alias reftest-assume-built)))

(rule
 (targets assume-built.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:assume-built.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-avoid-version)
 (action
  (diff avoid-version.test avoid-version.out)))

(alias
 (name reftest)
 (deps (alias reftest-avoid-version)))

(rule
 (targets avoid-version.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:avoid-version.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-best-effort)
 (action
  (diff best-effort.test best-effort.out)))

(alias
 (name reftest)
 (deps (alias reftest-best-effort)))

(rule
 (targets best-effort.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:best-effort.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-clean)
 (action
  (diff clean.test clean.out)))

(alias
 (name reftest)
 (deps (alias reftest-clean)))

(rule
 (targets clean.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:clean.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-cli-versioning)
 (action
  (diff cli-versioning.test cli-versioning.out)))

(alias
 (name reftest)
 (deps (alias reftest-cli-versioning)))

(rule
 (targets cli-versioning.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:cli-versioning.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-config)
 (action
  (diff config.test config.out)))

(alias
 (name reftest)
 (deps (alias reftest-config)))

(rule
 (targets config.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:config.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-4373)
 (action
  (diff conflict-4373.test conflict-4373.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-4373)))

(rule
 (targets conflict-4373.out)
 (deps root-c1d23f0e)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-4373.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-badversion)
 (action
  (diff conflict-badversion.test conflict-badversion.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-badversion)))

(rule
 (targets conflict-badversion.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-badversion.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-camlp4)
 (action
  (diff conflict-camlp4.test conflict-camlp4.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-camlp4)))

(rule
 (targets conflict-camlp4.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-camlp4.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-core)
 (action
  (diff conflict-core.test conflict-core.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-core)))

(rule
 (targets conflict-core.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-core.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-resto)
 (action
  (diff conflict-resto.test conflict-resto.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-resto)))

(rule
 (targets conflict-resto.out)
 (deps root-a5d7cdc0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-resto.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-conflict-solo5)
 (action
  (diff conflict-solo5.test conflict-solo5.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-solo5)))

(rule
 (targets conflict-solo5.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:conflict-solo5.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-cudf-preprocess)
 (action
  (diff cudf-preprocess.test cudf-preprocess.out)))

(alias
 (name reftest)
 (deps (alias reftest-cudf-preprocess)))

(rule
 (targets cudf-preprocess.out)
 (deps root-ad4dd344)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:cudf-preprocess.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-deprecated)
 (action
  (diff deprecated.test deprecated.out)))

(alias
 (name reftest)
 (deps (alias reftest-deprecated)))

(rule
 (targets deprecated.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:deprecated.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-deps-only)
 (action
  (diff deps-only.test deps-only.out)))

(alias
 (name reftest)
 (deps (alias reftest-deps-only)))

(rule
 (targets deps-only.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:deps-only.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-dot-install)
 (action
  (diff dot-install.test dot-install.out)))

(alias
 (name reftest)
 (deps (alias reftest-dot-install)))

(rule
 (targets dot-install.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:dot-install.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-001)
 (action
  (diff empty-conflicts-001.test empty-conflicts-001.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-001)))

(rule
 (targets empty-conflicts-001.out)
 (deps root-297366c)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-001.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-002)
 (action
  (diff empty-conflicts-002.test empty-conflicts-002.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-002)))

(rule
 (targets empty-conflicts-002.out)
 (deps root-11ea1cb)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-002.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-003)
 (action
  (diff empty-conflicts-003.test empty-conflicts-003.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-003)))

(rule
 (targets empty-conflicts-003.out)
 (deps root-3235916)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-003.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-004)
 (action
  (diff empty-conflicts-004.test empty-conflicts-004.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-004)))

(rule
 (targets empty-conflicts-004.out)
 (deps root-0070613707)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-004.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-005)
 (action
  (diff empty-conflicts-005.test empty-conflicts-005.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-005)))

(rule
 (targets empty-conflicts-005.out)
 (deps root-de897adf36c4230dfea812f40c98223b31c4521a)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-005.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-empty-conflicts-006)
 (action
  (diff empty-conflicts-006.test empty-conflicts-006.out)))

(alias
 (name reftest)
 (deps (alias reftest-empty-conflicts-006)))

(rule
 (targets empty-conflicts-006.out)
 (deps root-c1842d168d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:empty-conflicts-006.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-env)
 (action
  (diff env.test env.out)))

(alias
 (name reftest)
 (deps (alias reftest-env)))

(rule
 (targets env.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:env.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-init)
 (action
  (diff init.test init.out)))

(alias
 (name reftest)
 (deps (alias reftest-init)))

(rule
 (targets init.out)
 (deps root-009e00fa)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:init.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-inplace)
 (action
  (diff inplace.test inplace.out)))

(alias
 (name reftest)
 (deps (alias reftest-inplace)))

(rule
 (targets inplace.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:inplace.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-install-formula)
 (action
  (diff install-formula.test install-formula.out)))

(alias
 (name reftest)
 (deps (alias reftest-install-formula)))

(rule
 (targets install-formula.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:install-formula.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-install-pgocaml)
 (action
  (diff install-pgocaml.test install-pgocaml.out)))

(alias
 (name reftest)
 (deps (alias reftest-install-pgocaml)))

(rule
 (targets install-pgocaml.out)
 (deps root-f372039d)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:install-pgocaml.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-json.unix)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (diff json.unix.test json.unix.out)))

(alias
 (name reftest)
 (enabled_if (= %{os_type} "Unix"))
 (deps (alias reftest-json.unix)))

(rule
 (targets json.unix.out)
 (deps root-N0REP0)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:json.unix.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-legacy-git)
 (action
  (diff legacy-git.test legacy-git.out)))

(alias
 (name reftest)
 (deps (alias reftest-legacy-git)))

(rule
 (targets legacy-git.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:legacy-git.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-legacy-local)
 (action
  (diff legacy-local.test legacy-local.out)))

(alias
 (name reftest)
 (deps (alias reftest-legacy-local)))

(rule
 (targets legacy-local.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:legacy-local.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-lint)
 (action
  (diff lint.test lint.out)))

(alias
 (name reftest)
 (deps (alias reftest-lint)))

(rule
 (targets lint.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:lint.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-list)
 (action
  (diff list.test list.out)))

(alias
 (name reftest)
 (deps (alias reftest-list)))

(rule
 (targets list.out)
 (deps root-7371c1d9)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:list.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-list.unix)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (diff list.unix.test list.unix.out)))

(alias
 (name reftest)
 (enabled_if (= %{os_type} "Unix"))
 (deps (alias reftest-list.unix)))

(rule
 (targets list.unix.out)
 (deps root-f372039d)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:list.unix.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-lock)
 (action
  (diff lock.test lock.out)))

(alias
 (name reftest)
 (deps (alias reftest-lock)))

(rule
 (targets lock.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:lock.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-opamroot-versions)
 (action
  (diff opamroot-versions.test opamroot-versions.out)))

(alias
 (name reftest)
 (deps (alias reftest-opamroot-versions)))

(rule
 (targets opamroot-versions.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:opamroot-versions.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-opamrt-big-upgrade)
 (action
  (diff opamrt-big-upgrade.test opamrt-big-upgrade.out)))

(alias
 (name reftest)
 (deps (alias reftest-opamrt-big-upgrade)))

(rule
 (targets opamrt-big-upgrade.out)
 (deps root-7090735c)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:opamrt-big-upgrade.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-opamrt-dep-cycle)
 (action
  (diff opamrt-dep-cycle.test opamrt-dep-cycle.out)))

(alias
 (name reftest)
 (deps (alias reftest-opamrt-dep-cycle)))

(rule
 (targets opamrt-dep-cycle.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:opamrt-dep-cycle.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-opamrt-reinstall)
 (action
  (diff opamrt-reinstall.test opamrt-reinstall.out)))

(alias
 (name reftest)
 (deps (alias reftest-opamrt-reinstall)))

(rule
 (targets opamrt-reinstall.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:opamrt-reinstall.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-orphans)
 (action
  (diff orphans.test orphans.out)))

(alias
 (name reftest)
 (deps (alias reftest-orphans)))

(rule
 (targets orphans.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:orphans.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-parallel)
 (action
  (diff parallel.test parallel.out)))

(alias
 (name reftest)
 (deps (alias reftest-parallel)))

(rule
 (targets parallel.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:parallel.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-pat-sub)
 (action
  (diff pat-sub.test pat-sub.out)))

(alias
 (name reftest)
 (deps (alias reftest-pat-sub)))

(rule
 (targets pat-sub.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:pat-sub.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-pin)
 (action
  (diff pin.test pin.out)))

(alias
 (name reftest)
 (deps (alias reftest-pin)))

(rule
 (targets pin.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:pin.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-rebuild)
 (action
  (diff rebuild.test rebuild.out)))

(alias
 (name reftest)
 (deps (alias reftest-rebuild)))

(rule
 (targets rebuild.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:rebuild.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-rec-pin)
 (action
  (diff rec-pin.test rec-pin.out)))

(alias
 (name reftest)
 (deps (alias reftest-rec-pin)))

(rule
 (targets rec-pin.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:rec-pin.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-reinstall)
 (action
  (diff reinstall.test reinstall.out)))

(alias
 (name reftest)
 (deps (alias reftest-reinstall)))

(rule
 (targets reinstall.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:reinstall.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-remove)
 (action
  (diff remove.test remove.out)))

(alias
 (name reftest)
 (deps (alias reftest-remove)))

(rule
 (targets remove.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:remove.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-repository)
 (action
  (diff repository.test repository.out)))

(alias
 (name reftest)
 (deps (alias reftest-repository)))

(rule
 (targets repository.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:repository.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-resolve-variables)
 (action
  (diff resolve-variables.test resolve-variables.out)))

(alias
 (name reftest)
 (deps (alias reftest-resolve-variables)))

(rule
 (targets resolve-variables.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:resolve-variables.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-shared-fetch)
 (action
  (diff shared-fetch.test shared-fetch.out)))

(alias
 (name reftest)
 (deps (alias reftest-shared-fetch)))

(rule
 (targets shared-fetch.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:shared-fetch.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-show)
 (action
  (diff show.test show.out)))

(alias
 (name reftest)
 (deps (alias reftest-show)))

(rule
 (targets show.out)
 (deps root-009e00fa)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:show.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-source)
 (action
  (diff source.test source.out)))

(alias
 (name reftest)
 (deps (alias reftest-source)))

(rule
 (targets source.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:source.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-switch-creation)
 (action
  (diff switch-creation.test switch-creation.out)))

(alias
 (name reftest)
 (deps (alias reftest-switch-creation)))

(rule
 (targets switch-creation.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:switch-creation.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-switch-import)
 (action
  (diff switch-import.test switch-import.out)))

(alias
 (name reftest)
 (deps (alias reftest-switch-import)))

(rule
 (targets switch-import.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:switch-import.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-switch-invariant)
 (action
  (diff switch-invariant.test switch-invariant.out)))

(alias
 (name reftest)
 (deps (alias reftest-switch-invariant)))

(rule
 (targets switch-invariant.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:switch-invariant.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-switch-set)
 (action
  (diff switch-set.test switch-set.out)))

(alias
 (name reftest)
 (deps (alias reftest-switch-set)))

(rule
 (targets switch-set.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:switch-set.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-tree)
 (action
  (diff tree.test tree.out)))

(alias
 (name reftest)
 (deps (alias reftest-tree)))

(rule
 (targets tree.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:tree.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-unhelpful-conflicts-001)
 (action
  (diff unhelpful-conflicts-001.test unhelpful-conflicts-001.out)))

(alias
 (name reftest)
 (deps (alias reftest-unhelpful-conflicts-001)))

(rule
 (targets unhelpful-conflicts-001.out)
 (deps root-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:unhelpful-conflicts-001.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-update-upgrade)
 (action
  (diff update-upgrade.test update-upgrade.out)))

(alias
 (name reftest)
 (deps (alias reftest-update-upgrade)))

(rule
 (targets update-upgrade.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:update-upgrade.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-upgrade-format)
 (action
  (diff upgrade-format.test upgrade-format.out)))

(alias
 (name reftest)
 (deps (alias reftest-upgrade-format)))

(rule
 (targets upgrade-format.out)
 (deps root-009e00fa)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:upgrade-format.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-upgrade-two-point-o)
 (action
  (diff upgrade-two-point-o.test upgrade-two-point-o.out)))

(alias
 (name reftest)
 (deps (alias reftest-upgrade-two-point-o)))

(rule
 (targets upgrade-two-point-o.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:upgrade-two-point-o.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-upgrade)
 (action
  (diff upgrade.test upgrade.out)))

(alias
 (name reftest)
 (deps (alias reftest-upgrade)))

(rule
 (targets upgrade.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:upgrade.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-var-option)
 (action
  (diff var-option.test var-option.out)))

(alias
 (name reftest)
 (deps (alias reftest-var-option)))

(rule
 (targets var-option.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:var-option.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-with-tools)
 (action
  (diff with-tools.test with-tools.out)))

(alias
 (name reftest)
 (deps (alias reftest-with-tools)))

(rule
 (targets with-tools.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:with-tools.test} %{read-lines:testing-env}))))

(rule
 (alias reftest-working-dir)
 (action
  (diff working-dir.test working-dir.out)))

(alias
 (name reftest)
 (deps (alias reftest-working-dir)))

(rule
 (targets working-dir.out)
 (deps root-N0REP0)
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:working-dir.test} %{read-lines:testing-env}))))

(rule
 (targets opam-repo-N0REP0)
 (action
  (progn
   (run mkdir -p %{targets}/packages)
   (write-file repo "opam-version:\"2.0\"")
   (run cp repo %{targets}/repo))))

(rule
  (targets root-N0REP0)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-N0REP0})))))

(rule
 (targets opam-archive-0070613707.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/0070613707.tar.gz)))

(rule
  (targets opam-repo-0070613707)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-0070613707.tar.gz} --strip-components=1))))

(rule
  (targets root-0070613707)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-0070613707})))))

(rule
 (targets opam-archive-009e00fa.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/009e00fa.tar.gz)))

(rule
  (targets opam-repo-009e00fa)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-009e00fa.tar.gz} --strip-components=1))))

(rule
  (targets root-009e00fa)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-009e00fa})))))

(rule
 (targets opam-archive-11ea1cb.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/11ea1cb.tar.gz)))

(rule
  (targets opam-repo-11ea1cb)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-11ea1cb.tar.gz} --strip-components=1))))

(rule
  (targets root-11ea1cb)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-11ea1cb})))))

(rule
 (targets opam-archive-297366c.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/297366c.tar.gz)))

(rule
  (targets opam-repo-297366c)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-297366c.tar.gz} --strip-components=1))))

(rule
  (targets root-297366c)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-297366c})))))

(rule
 (targets opam-archive-3235916.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/3235916.tar.gz)))

(rule
  (targets opam-repo-3235916)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-3235916.tar.gz} --strip-components=1))))

(rule
  (targets root-3235916)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-3235916})))))

(rule
 (targets opam-archive-7090735c.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/7090735c.tar.gz)))

(rule
  (targets opam-repo-7090735c)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-7090735c.tar.gz} --strip-components=1))))

(rule
  (targets root-7090735c)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-7090735c})))))

(rule
 (targets opam-archive-7371c1d9.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/7371c1d9.tar.gz)))

(rule
  (targets opam-repo-7371c1d9)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-7371c1d9.tar.gz} --strip-components=1))))

(rule
  (targets root-7371c1d9)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-7371c1d9})))))

(rule
 (targets opam-archive-a5d7cdc0.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/a5d7cdc0.tar.gz)))

(rule
  (targets opam-repo-a5d7cdc0)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-a5d7cdc0.tar.gz} --strip-components=1))))

(rule
  (targets root-a5d7cdc0)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-a5d7cdc0})))))

(rule
 (targets opam-archive-ad4dd344.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/ad4dd344.tar.gz)))

(rule
  (targets opam-repo-ad4dd344)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-ad4dd344.tar.gz} --strip-components=1))))

(rule
  (targets root-ad4dd344)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-ad4dd344})))))

(rule
 (targets opam-archive-c1842d168d.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/c1842d168d.tar.gz)))

(rule
  (targets opam-repo-c1842d168d)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-c1842d168d.tar.gz} --strip-components=1))))

(rule
  (targets root-c1842d168d)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-c1842d168d})))))

(rule
 (targets opam-archive-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a.tar.gz)))

(rule
  (targets opam-repo-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a.tar.gz} --strip-components=1))))

(rule
  (targets root-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a})))))

(rule
 (targets opam-archive-c1d23f0e.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/c1d23f0e.tar.gz)))

(rule
  (targets opam-repo-c1d23f0e)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-c1d23f0e.tar.gz} --strip-components=1))))

(rule
  (targets root-c1d23f0e)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-c1d23f0e})))))

(rule
 (targets opam-archive-de897adf36c4230dfea812f40c98223b31c4521a.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/de897adf36c4230dfea812f40c98223b31c4521a.tar.gz)))

(rule
  (targets opam-repo-de897adf36c4230dfea812f40c98223b31c4521a)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-de897adf36c4230dfea812f40c98223b31c4521a.tar.gz} --strip-components=1))))

(rule
  (targets root-de897adf36c4230dfea812f40c98223b31c4521a)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-de897adf36c4230dfea812f40c98223b31c4521a})))))

(rule
 (targets opam-archive-f372039d.tar.gz)
 (action (run curl --silent -Lo %{targets} https://github.com/ocaml/opam-repository/archive/f372039d.tar.gz)))

(rule
  (targets opam-repo-f372039d)
  (action
   (progn
    (run mkdir -p %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-f372039d.tar.gz} --strip-components=1))))

(rule
  (targets root-f372039d)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-f372039d})))))
