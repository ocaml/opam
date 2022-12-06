
(alias
 (name reftest-avoid-version)
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

(alias
 (name reftest-cli-versioning)
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

(alias
 (name reftest-conflict-4373)
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

(alias
 (name reftest-conflict-badversion)
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

(alias
 (name reftest-conflict-camlp4)
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

(alias
 (name reftest-conflict-core)
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

(alias
 (name reftest-conflict-resto)
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

(alias
 (name reftest-conflict-solo5)
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

(alias
 (name reftest-cudf-preprocess)
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

(alias
 (name reftest-dot-install)
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

(alias
 (name reftest-empty-conflicts-001)
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

(alias
 (name reftest-empty-conflicts-002)
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

(alias
 (name reftest-empty-conflicts-003)
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

(alias
 (name reftest-empty-conflicts-004)
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

(alias
 (name reftest-empty-conflicts-005)
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

(alias
 (name reftest-empty-conflicts-006)
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

(alias
 (name reftest-env)
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

(alias
 (name reftest-init)
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

(alias
 (name reftest-install-pgocaml)
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

(alias
 (name reftest-legacy-git)
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

(alias
 (name reftest-legacy-local)
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

(alias
 (name reftest-list.unix)
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

(alias
 (name reftest-opamroot-versions)
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

(alias
 (name reftest-opamrt-big-upgrade)
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

(alias
 (name reftest-opamrt-dep-cycle)
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

(alias
 (name reftest-opamrt-reinstall)
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

(alias
 (name reftest-orphans)
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

(alias
 (name reftest-pat-sub)
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

(alias
 (name reftest-pin)
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

(alias
 (name reftest-pin.unix)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (diff pin.unix.test pin.unix.out)))

(alias
 (name reftest)
 (enabled_if (= %{os_type} "Unix"))
 (deps (alias reftest-pin.unix)))

(rule
 (targets pin.unix.out)
 (deps root-N0REP0)
 (enabled_if (= %{os_type} "Unix"))
 (action
  (with-stdout-to
   %{targets}
   (run ./run.exe %{bin:opam} %{dep:pin.unix.test} %{read-lines:testing-env}))))

(alias
 (name reftest-remove)
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

(alias
 (name reftest-show)
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

(alias
 (name reftest-switch-creation)
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

(alias
 (name reftest-switch-invariant)
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

(alias
 (name reftest-upgrade-format)
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

(alias
 (name reftest-upgrade-two-point-o)
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

(alias
 (name reftest-var-option)
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

(alias
 (name reftest-working-dir)
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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/0070613707.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/009e00fa.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/11ea1cb.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/297366c.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/3235916.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/7090735c.tar.gz)))

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
 (targets opam-archive-a5d7cdc0.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/a5d7cdc0.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/ad4dd344.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/c1842d168d.tar.gz)))

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
 (targets opam-archive-c1d23f0e.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/c1d23f0e.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/de897adf36c4230dfea812f40c98223b31c4521a.tar.gz)))

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
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/f372039d.tar.gz)))

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
