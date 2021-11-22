. .github/scripts/common/preamble.sh

if [ "$GITHUB_EVENT_NAME" = "pull_request" ] && [ "x" = "x$BASE_REF_SHA$PR_REF_SHA" ] ; then
  echo "Variables BASE_REF_SHA and PR_REF_SHA must be defined in a pull request job"
  exit 2
fi
# Don't use BASE_REF_SHA and PR_REF_SHA on non pull request jobs, they are not
# defined. See .github/workflows/ci.yml hygiene job.

if [ "$GITHUB_EVENT_NAME" = "pull_request" ]; then
  # needed for git diffs and rev-list
  # we need to get history from base ref to head ref for check configure
  depth=10
  set +e
  git cat-file -e $BASE_REF_SHA
  r=$?
  while [ $r -ne 0 ] ; do
    git fetch origin $GITHUB_REF --depth=$depth
    depth=$(( $depth + 10 ))
    git cat-file -e $BASE_REF_SHA
    r=$?
  done
  set -e
fi

set +x

ERROR=0
