# Releasing Livebook

0. If applicable, release Kino first and update built-in notebooks to reference
   the new version.
1. Make sure the latest CI "Assets" workflow finished.
2. Switch to (or create) vx.y branch.
3. If applicable cherry-pick the relevant commits from main onto the vx.y branch.
   1. If you do that, push, wait for CI "Assets" workflow to finish, and pull.
3. Update version in `mix.exs` and finish changelog.
4. Run `mix hex.build` as a sanity check.
5. `git tag vx.y.z`, `git push --tags`
   1. Wait for CI to finish (Docker and Desktop)
6. Run `mix hex.publish`.
7. Publish GH release with copied changelog notes.
8. If you created a branch in step 2., update main changelog to point to
   the branch and bump version in mix.exs (with `-dev` suffix).
