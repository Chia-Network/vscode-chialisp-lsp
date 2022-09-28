#!/usr/bin/env bash
set -e

## Description: Sets usable env for all job steps based on payload in GitHub Event. To be ran immediately after 'actions/checkout' step

# Checking dependencies
if ! command -v jq &> /dev/null; then
  echo "ERROR: Missing jq"
  echo "Install it from your package manager, or get it at https://github.com/stedolan/jq"
  exit 1
fi

echo "GitHub native job envs:"
echo "――――――――――――――――――――――"
echo "CI="$CI
echo "GITHUB_ACTION="$GITHUB_ACTION
echo "GITHUB_ACTION_PATH="$GITHUB_ACTION_PATH
echo "GITHUB_ACTION_REPOSITORY="$GITHUB_ACTION_REPOSITORY
echo "GITHUB_ACTIONS="$GITHUB_ACTIONS
echo "GITHUB_ACTOR="$GITHUB_ACTOR
echo "GITHUB_API_URL="$GITHUB_API_URL
echo "GITHUB_BASE_REF="$GITHUB_BASE_REF
echo "GITHUB_ENV="$GITHUB_ENV
echo "GITHUB_EVENT_NAME="$GITHUB_EVENT_NAME
echo "GITHUB_EVENT_PATH="$GITHUB_EVENT_PATH
echo "GITHUB_GRAPHQL_URL="$GITHUB_GRAPHQL_URL
echo "GITHUB_HEAD_REF="$GITHUB_HEAD_REF
echo "GITHUB_JOB="$GITHUB_JOB
echo "GITHUB_PATH="$GITHUB_PATH
echo "GITHUB_REF="$GITHUB_REF
echo "GITHUB_REF_NAME="$GITHUB_REF_NAME
echo "GITHUB_REF_PROTECTED="$GITHUB_REF_PROTECTED
echo "GITHUB_REF_TYPE="$GITHUB_REF_TYPE
echo "GITHUB_REPOSITORY="$GITHUB_REPOSITORY
echo "GITHUB_REPOSITORY_OWNER="$GITHUB_REPOSITORY_OWNER
echo "GITHUB_RETENTION_DAYS="$GITHUB_RETENTION_DAYS
echo "GITHUB_RUN_ATTEMPT="$GITHUB_RUN_ATTEMPT
echo "GITHUB_RUN_ID="$GITHUB_RUN_ID
echo "GITHUB_RUN_NUMBER="$GITHUB_RUN_NUMBER
echo "GITHUB_SERVER_URL="$GITHUB_SERVER_URL
echo "GITHUB_SHA="$GITHUB_SHA
echo "GITHUB_STEP_SUMMARY="$GITHUB_STEP_SUMMARY
echo "GITHUB_WORKFLOW="$GITHUB_WORKFLOW
echo "GITHUB_WORKSPACE="$GITHUB_WORKSPACE
echo "RUNNER_ARCH="$RUNNER_ARCH
echo "RUNNER_DEBUG="$RUNNER_DEBUG
echo "RUNNER_NAME="$RUNNER_NAME
echo "RUNNER_OS="$RUNNER_OS
echo "RUNNER_TEMP="$RUNNER_TEMP
echo "RUNNER_TOOL_CACHE="$RUNNER_TOOL_CACHE
echo "――――――――――――――――――――――"


# These are needed for some of the functions and conditionals within this action
REPO=$(echo "$GITHUB_REPOSITORY" | cut -d '/' -f2)
LANGUAGE=$(curl -s -X GET https://"$GPR_TOKEN":@api.github.com/repos/"$GITHUB_REPOSITORY" | jq -r '.language')
RFC_REPO=$(echo "$REPO" | sed 's/\./-/g; s/_/-/g')
GIT_SHORT_HASH=$(echo "${GITHUB_SHA}" | cut -c1-8)

# Pre-Release/Release logic
PRE_RELEASE=$(jq -r '.release.prerelease' "$GITHUB_EVENT_PATH")
RELEASE_TAG=$(jq -r '.release.tag_name' "$GITHUB_EVENT_PATH")
{
  echo "RELEASE=true"
  echo "PRE_RELEASE=$PRE_RELEASE"
  echo "RELEASE_TAG=$RELEASE_TAG"
} >> "$GITHUB_ENV"
if [ "$PRE_RELEASE" = false ]; then
  echo "FULL_RELEASE=true" >>"$GITHUB_ENV"
else
  echo "FULL_RELEASE=false" >>"$GITHUB_ENV"
fi

# Begin custom
echo "Setting env for steps:"
echo "――――――――――――――――――――――"

echo "GITHUB_WORKFLOW_URL=$GITHUB_SERVER_URL/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID" >>$GITHUB_ENV
echo "Set GITHUB_WORKFLOW_URL=$GITHUB_WORKFLOW_URL"

# Repo name represented in a RFC1035 format - useful if you want the repo name = k8s namespace/service/deployment etc
echo "RFC_REPO=$RFC_REPO" >>$GITHUB_ENV
echo "Set RFC_REPO=$RFC_REPO"

echo "GIT_SHORT_HASH=$GIT_SHORT_HASH" >>$GITHUB_ENV
echo "Set GIT_SHORT_HASH=$GIT_SHORT_HASH"

# For publishing artifacts to GitHub Package Repository
export GPR_PROJECT=docker.pkg.github.com/$GITHUB_REPOSITORY_OWNER/$REPO
echo "GPR_PROJECT=$GPR_PROJECT" >>$GITHUB_ENV
echo "Set GPR_PROJECT=$GPR_PROJECT"

export REVISION=${GITHUB_REF##*/}
echo "REVISION=$REVISION" >>$GITHUB_ENV
echo "Set REVISION=$REVISION"

# # For Dockerhub, uncomment the following block
# export DHREPO_NAME=$GITHUB_REPOSITORY_OWNER/$REPO
# echo "DHREPO_NAME=$DHREPO_NAME" >>$GITHUB_ENV
# echo "Set DHREPO_NAME=$DHREPO_NAME"
#
# export DHIMAGE_TAG="$DHREPO_NAME:$GIT_SHORT_HASH"
# echo "DHIMAGE_TAG=$DHIMAGE_TAG" >>$GITHUB_ENV
# echo "Set DHIMAGE_TAG=$DHIMAGE_TAG"
#
# export DHIMAGE_VERSION="$RELEASE_VERSION"
# echo "DHIMAGE_VERSION=$DHIMAGE_VERSION" >>$GITHUB_ENV
# echo "Set DHIMAGE_VERSION=$DHIMAGE_VERSION"
#
# export DHIMAGE_TAG_VERSION="$DHREPO_NAME:$DHIMAGE_VERSION"
# echo "DHIMAGE_TAG_VERSION=$DHIMAGE_TAG_VERSION" >>$GITHUB_ENV
# echo "Set DHIMAGE_TAG_VERSION=$DHIMAGE_TAG_VERSION"
#
# export DOCKER_BUILDKIT=1
# echo "DOCKER_BUILDKIT=$DOCKER_BUILDKIT" >>$GITHUB_ENV
# echo "Set DOCKER_BUILDKIT=$DOCKER_BUILDKIT"
# #

# # If using remote Docker host, uncomment the following block
# export DOCKER_HOST=tcp://my-remote.docker-host.tld:2376
# echo "DOCKER_HOST=$DOCKER_HOST" >> $GITHUB_ENV
#
# export DOCKER_TLS_VERIFY=1
# echo "DOCKER_TLS_VERIFY=$DOCKER_TLS_VERIFY" >> $GITHUB_ENV

echo "――――――――――――――――――――――"
echo "Complete"
