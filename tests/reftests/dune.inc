
(alias
 (name reftest-camelus)
 (action
  (diff camelus.test camelus.out)))

(alias
 (name reftest)
 (deps (alias reftest-camelus)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   camelus.out
   (run ./run.exe %{bin:opam} %{dep:camelus.test} %{dep:root-c7f8804c}))))

(alias
 (name reftest-conflict-badversion)
 (action
  (diff conflict-badversion.test conflict-badversion.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-badversion)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   conflict-badversion.out
   (run ./run.exe %{bin:opam} %{dep:conflict-badversion.test} %{dep:root-f372039d}))))

(alias
 (name reftest-conflict-camlp4)
 (action
  (diff conflict-camlp4.test conflict-camlp4.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-camlp4)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   conflict-camlp4.out
   (run ./run.exe %{bin:opam} %{dep:conflict-camlp4.test} %{dep:root-f372039d}))))

(alias
 (name reftest-conflict-core)
 (action
  (diff conflict-core.test conflict-core.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-core)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   conflict-core.out
   (run ./run.exe %{bin:opam} %{dep:conflict-core.test} %{dep:root-f372039d}))))

(alias
 (name reftest-conflict-solo5)
 (action
  (diff conflict-solo5.test conflict-solo5.out)))

(alias
 (name reftest)
 (deps (alias reftest-conflict-solo5)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   conflict-solo5.out
   (run ./run.exe %{bin:opam} %{dep:conflict-solo5.test} %{dep:root-f372039d}))))

(alias
 (name reftest-install-pgocaml)
 (action
  (diff install-pgocaml.test install-pgocaml.out)))

(alias
 (name reftest)
 (deps (alias reftest-install-pgocaml)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   install-pgocaml.out
   (run ./run.exe %{bin:opam} %{dep:install-pgocaml.test} %{dep:root-f372039d}))))

(alias
 (name reftest-legacy-local)
 (action
  (diff legacy-local.test legacy-local.out)))

(alias
 (name reftest)
 (deps (alias reftest-legacy-local)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   legacy-local.out
   (run ./run.exe %{bin:opam} %{dep:legacy-local.test} %{dep:root-632bc2e}))))

(alias
 (name reftest-show)
 (action
  (diff show.test show.out)))

(alias
 (name reftest)
 (deps (alias reftest-show)))

(rule
 (locks ./opam)
 (action
  (with-stdout-to
   show.out
   (run ./run.exe %{bin:opam} %{dep:show.test} %{dep:root-009e00fa}))))

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
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-009e00fa}))))

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
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-632bc2e}))))

(rule
 (targets opam-archive-c7f8804c.tar.gz)
 (action (run wget --quiet -O %{targets} https://github.com/ocaml/opam-repository/archive/c7f8804c.tar.gz)))

(rule
  (targets opam-repo-c7f8804c)
  (action
   (progn
    (run mkdir %{targets})
    (run tar -C %{targets} -xzf %{dep:opam-archive-c7f8804c.tar.gz} --strip-components=1))))

(rule
  (targets root-c7f8804c)
  (action
   (progn
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-c7f8804c}))))

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
    (run %{bin:opam} init --root=%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%{dep:opam-repo-f372039d}))))
