
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
