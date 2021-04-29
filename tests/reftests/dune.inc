
(alias
 (name reftest-cli-versioning)
 (action
  (diff cli-versioning.test cli-versioning.out)))

(alias
 (name reftest)
 (deps (alias reftest-cli-versioning)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   cli-versioning.out
   (run ./run.exe %{bin:opam} %{dep:cli-versioning.test} %{read-lines:testing-env}))))

(alias
 (name reftest-conflict-4373)
 (action
  (diff conflict-4373.test conflict-4373.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-4373)))

(rule
 (deps root-c1d23f0e)
 (action
  (with-stdout-to
   conflict-4373.out
   (run ./run.exe %{bin:opam} %{dep:conflict-4373.test} %{read-lines:testing-env}))))

(alias
 (name reftest-conflict-badversion)
 (action
  (diff conflict-badversion.test conflict-badversion.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-badversion)))

(rule
 (deps root-f372039d)
 (action
  (with-stdout-to
   conflict-badversion.out
   (run ./run.exe %{bin:opam} %{dep:conflict-badversion.test} %{read-lines:testing-env}))))

(alias
 (name reftest-conflict-camlp4)
 (action
  (diff conflict-camlp4.test conflict-camlp4.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-camlp4)))

(rule
 (deps root-f372039d)
 (action
  (with-stdout-to
   conflict-camlp4.out
   (run ./run.exe %{bin:opam} %{dep:conflict-camlp4.test} %{read-lines:testing-env}))))

(alias
 (name reftest-conflict-core)
 (action
  (diff conflict-core.test conflict-core.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-core)))

(rule
 (deps root-f372039d)
 (action
  (with-stdout-to
   conflict-core.out
   (run ./run.exe %{bin:opam} %{dep:conflict-core.test} %{read-lines:testing-env}))))

(alias
 (name reftest-conflict-solo5)
 (action
  (diff conflict-solo5.test conflict-solo5.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-solo5)))

(rule
 (deps root-f372039d)
 (action
  (with-stdout-to
   conflict-solo5.out
   (run ./run.exe %{bin:opam} %{dep:conflict-solo5.test} %{read-lines:testing-env}))))

(alias
 (name reftest-cudf-preprocess)
 (action
  (diff cudf-preprocess.test cudf-preprocess.out)))

(alias
 (name reftest)
 (deps (alias reftest-cudf-preprocess)))

(rule
 (deps root-ad4dd344)
 (action
  (with-stdout-to
   cudf-preprocess.out
   (run ./run.exe %{bin:opam} %{dep:cudf-preprocess.test} %{read-lines:testing-env}))))

(alias
 (name reftest-dot-install)
 (action
  (diff dot-install.test dot-install.out)))

(alias
 (name reftest)
 (deps (alias reftest-dot-install)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   dot-install.out
   (run ./run.exe %{bin:opam} %{dep:dot-install.test} %{read-lines:testing-env}))))

(alias
 (name reftest-init)
 (action
  (diff init.test init.out)))

(alias
 (name reftest)
 (deps (alias reftest-init)))

(rule
 (deps root-009e00fa)
 (action
  (with-stdout-to
   init.out
   (run ./run.exe %{bin:opam} %{dep:init.test} %{read-lines:testing-env}))))

(alias
 (name reftest-install-pgocaml)
 (action
  (diff install-pgocaml.test install-pgocaml.out)))

(alias
 (name reftest)
 (deps (alias reftest-install-pgocaml)))

(rule
 (deps root-f372039d)
 (action
  (with-stdout-to
   install-pgocaml.out
   (run ./run.exe %{bin:opam} %{dep:install-pgocaml.test} %{read-lines:testing-env}))))

(alias
 (name reftest-legacy-git)
 (action
  (diff legacy-git.test legacy-git.out)))

(alias
 (name reftest)
 (deps (alias reftest-legacy-git)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   legacy-git.out
   (run ./run.exe %{bin:opam} %{dep:legacy-git.test} %{read-lines:testing-env}))))

(alias
 (name reftest-legacy-local)
 (action
  (diff legacy-local.test legacy-local.out)))

(alias
 (name reftest)
 (deps (alias reftest-legacy-local)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   legacy-local.out
   (run ./run.exe %{bin:opam} %{dep:legacy-local.test} %{read-lines:testing-env}))))

(alias
 (name reftest-opamroot-versions)
 (action
  (diff opamroot-versions.test opamroot-versions.out)))

(alias
 (name reftest)
 (deps (alias reftest-opamroot-versions)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   opamroot-versions.out
   (run ./run.exe %{bin:opam} %{dep:opamroot-versions.test} %{read-lines:testing-env}))))

(alias
 (name reftest-pat-sub)
 (action
  (diff pat-sub.test pat-sub.out)))

(alias
 (name reftest)
 (deps (alias reftest-pat-sub)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   pat-sub.out
   (run ./run.exe %{bin:opam} %{dep:pat-sub.test} %{read-lines:testing-env}))))

(alias
 (name reftest-show)
 (action
  (diff show.test show.out)))

(alias
 (name reftest)
 (deps (alias reftest-show)))

(rule
 (deps root-009e00fa)
 (action
  (with-stdout-to
   show.out
   (run ./run.exe %{bin:opam} %{dep:show.test} %{read-lines:testing-env}))))

(alias
 (name reftest-switch-creation)
 (action
  (diff switch-creation.test switch-creation.out)))

(alias
 (name reftest)
 (deps (alias reftest-switch-creation)))

(rule
 (deps root-632bc2e)
 (action
  (with-stdout-to
   switch-creation.out
   (run ./run.exe %{bin:opam} %{dep:switch-creation.test} %{read-lines:testing-env}))))

(alias
 (name reftest-upgrade-format)
 (action
  (diff upgrade-format.test upgrade-format.out)))

(alias
 (name reftest)
 (deps (alias reftest-upgrade-format)))

(rule
 (deps root-009e00fa)
 (action
  (with-stdout-to
   upgrade-format.out
   (run ./run.exe %{bin:opam} %{dep:upgrade-format.test} %{read-lines:testing-env}))))

(alias
 (name reftest-var-option)
 (action
  (diff var-option.test var-option.out)))

(alias
 (name reftest)
 (deps (alias reftest-var-option)))

(rule
 (deps root-009e00fa)
 (action
  (with-stdout-to
   var-option.out
   (run ./run.exe %{bin:opam} %{dep:var-option.test} %{read-lines:testing-env}))))

(rule
 (targets opam-archive-009e00fa.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/009e00fa.tar.gz)))

(rule
  (targets opam-repo-009e00fa)
  (action
   (progn
    (run mkdir %{targets})
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
 (targets opam-archive-632bc2e.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/632bc2e.tar.gz)))

(rule
  (targets opam-repo-632bc2e)
  (action
   (progn
    (run mkdir %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-632bc2e.tar.gz} --strip-components=1))))

(rule
  (targets root-632bc2e)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-632bc2e})))))

(rule
 (targets opam-archive-ad4dd344.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/ad4dd344.tar.gz)))

(rule
  (targets opam-repo-ad4dd344)
  (action
   (progn
    (run mkdir %{targets})
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
    (run mkdir %{targets})
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
    (run mkdir %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-f372039d.tar.gz} --strip-components=1))))

(rule
  (targets root-f372039d)
  (action
   (progn
    (ignore-stdout
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-f372039d})))))
