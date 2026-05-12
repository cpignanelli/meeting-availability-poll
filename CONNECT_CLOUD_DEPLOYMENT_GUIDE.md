# Deploy This App To GitHub And Posit Connect Cloud

This guide explains how to publish the Shiny meeting availability poll app from a GitHub repository to Posit Connect Cloud. It is intentionally generic: replace placeholders such as `<owner>`, `<repo>`, and `https://your-connect-app.example/` with your own values outside of version control.

Do not commit real participant data, local SQLite databases, `.Renviron`, API keys, SMTP passwords, or other secrets.

## What You Are Publishing

You are publishing the app code only. You are not publishing the local proof-of-concept database.

Commit these files and folders:

```text
app.R
manifest.json
renv.lock
R/
www/
data/.gitkeep
scripts/
tests/
README.md
CONNECT_CLOUD_DEPLOYMENT_GUIDE.md
.gitignore
.rscignore
.Renviron.example
```

Do not commit:

```text
data/app.sqlite
.Renviron
.Rhistory
.RData
.DS_Store
```

## Prepare The Project

From the project root, run:

```sh
Rscript tests/testthat.R
Rscript scripts/write_connect_cloud_manifest.R
```

`manifest.json` is required by Posit Connect Cloud so it can install the correct R dependencies.

## Publish From GitHub

1. Create a GitHub repository, for example:

```text
https://github.com/<owner>/<repo>.git
```

2. Connect the local project to GitHub:

```sh
git remote add origin https://github.com/<owner>/<repo>.git
git branch -M main
git push -u origin main
```

If `origin` already exists, inspect it with `git remote -v` and update it if needed:

```sh
git remote set-url origin https://github.com/<owner>/<repo>.git
```

3. In Posit Connect Cloud, click **Publish**.
4. Choose **Shiny**.
5. Select the GitHub repository and branch.
6. Select `app.R` as the primary file.
7. Confirm that Connect Cloud detects `manifest.json`.

Free Connect Cloud workflows may require a public GitHub repository. Use low-risk test data only unless the app is deployed with production-grade access controls and durable storage.

## Configure Environment Variables

In the app content settings or publish workflow, add:

```text
SQLITE_DB_PATH=data/app.sqlite
APP_BASE_URL=https://your-connect-app.example/
ORGANIZER_AUTH_SECRET=<a-long-random-secret>
SMTP_HOST=<smtp-host>
SMTP_PORT=587
SMTP_USERNAME=you@example.org
SMTP_PASSWORD=<smtp-password>
SMTP_FROM=you@example.org
SMTP_USE_SSL=false
ALLOW_DEV_AUTH_CODE_DISPLAY=false
```

Optional legacy fallback:

```text
POLL_CREATION_SECRET=<a-long-random-secret>
```

`APP_BASE_URL` should be the final public app URL, including the trailing `/`. If you do not know the final URL before the first publish, add it after the app is published, then restart or republish the content.

For local testing only, you may leave SMTP blank and set:

```text
ALLOW_DEV_AUTH_CODE_DISPLAY=true
```

Do not enable development code display on a public deployment.

## Test The Deployed App

1. Open the deployed root URL:

```text
https://your-connect-app.example/
```

2. Request an organizer login code.
3. Enter the code and open the **Create poll** tab.
4. Create a test poll.
5. Share only the generated public `?respond=<token>` link with participants.
6. Keep the generated private `?admin=<token>` organizer link as backup access.

Participants can respond through the effective expiry date. By default, that is the final proposed meeting date. If an earlier response deadline is set, that earlier date is used.

## SQLite Pilot Caveats

SQLite is suitable for local development and short proof-of-concept testing only. It is not durable production persistence on Connect Cloud because redeployments or restarts can remove app-local files.

Before production use:

- move persistence to a hosted database such as PostgreSQL, Supabase, Neon, Azure SQL, or another managed database;
- keep secrets in platform environment variables;
- define a retention/deletion policy for personal information;
- add platform-level rate limiting or other abuse controls;
- use stronger organization authentication if multiple organizers manage sensitive polls.

## Troubleshooting

### The repository does not show up in Connect Cloud

Confirm that the repository is accessible to Posit Connect Cloud and that the Posit GitHub App has permission to read it.

### Connect Cloud says dependency information is missing

Run:

```sh
Rscript scripts/write_connect_cloud_manifest.R
git add manifest.json
git commit -m "Update Connect Cloud manifest"
git push
```

Then republish.

### Poll links point to localhost

Set `APP_BASE_URL` to the deployed app URL, then restart or republish.

### Organizer login does not send email

Confirm that `ORGANIZER_AUTH_SECRET` and all SMTP variables are set in the deployed content settings. Keep `ALLOW_DEV_AUTH_CODE_DISPLAY=false` on public deployments.

### Poll data disappeared

This is a known limitation of SQLite proof-of-concept deployments. Export results before redeploying, or move to a hosted database before collecting important data.
