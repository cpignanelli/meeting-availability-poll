# Deploy This App To GitHub And Posit Connect Cloud

This guide explains how to publish the Shiny meeting availability poll app from a GitHub repository to Posit Connect Cloud. It is intentionally generic: replace placeholders such as `<owner>`, `<repo>`, and `https://your-connect-app.example/` with your own values outside of version control.

Do not commit real participant data, local SQLite databases, `.Renviron`, API keys, SMTP passwords, or other secrets.

## What You Are Publishing

You are publishing the app code only. You are not publishing the local proof-of-concept database or any secret database credentials.

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
APP_BASE_URL=https://your-connect-app.example/
APP_MAIN_OWNER_EMAIL=owner@example.org
APP_AUTH_SECRET=<a-long-random-secret>
ORGANIZER_AUTH_SECRET=<a-long-random-secret>
TRUSTED_SESSION_MINUTES=10
SMTP_HOST=<smtp-host>
SMTP_PORT=587
SMTP_USERNAME=you@example.org
SMTP_PASSWORD=<smtp-password>
SMTP_FROM=you@example.org
SMTP_USE_SSL=false
ALLOW_DEV_AUTH_CODE_DISPLAY=false
```

`APP_AUTH_SECRET` is preferred for new deployments. `ORGANIZER_AUTH_SECRET` can remain set for backward compatibility with older deployments.

For MongoDB Atlas hosted persistence, add these variables too:

```text
DATABASE_BACKEND=mongodb
MONGODB_URI=<mongodb+srv connection string>
MONGODB_DATABASE=meeting_poll
```

Save `MONGODB_URI` as a secret. It contains the database username and password and must never be committed to Git.

For a short SQLite-only proof of concept, use this instead:

```text
DATABASE_BACKEND=sqlite
SQLITE_DB_PATH=data/app.sqlite
```

Optional legacy fallback:

```text
POLL_CREATION_SECRET=<a-long-random-secret>
```

`APP_BASE_URL` should be the final public app URL, including the trailing `/`. If you do not know the final URL before the first publish, add it after the app is published, then restart or republish the content.

`APP_MAIN_OWNER_EMAIL` is required. That email receives organizer access request notifications and is the only account that can approve, deny, or revoke secondary organizer access.

For local testing only, you may leave SMTP blank and set:

```text
ALLOW_DEV_AUTH_CODE_DISPLAY=true
```

Do not enable development code display on a public deployment.

## MongoDB Atlas Checklist

Use MongoDB Atlas Free when you want a no-cost hosted database for live pilot testing on Posit Connect Cloud.

1. Create a Free cluster.
2. Create a database user for the app.
3. Give that user read/write access to the app database.
4. Add the Posit Connect Cloud outbound IP addresses to Atlas Network Access.
5. Copy the MongoDB SRV connection string and replace the password placeholder.
6. In Connect Cloud variables, set:

```text
DATABASE_BACKEND=mongodb
MONGODB_URI=<mongodb+srv connection string>
MONGODB_DATABASE=meeting_poll
```

7. Restart or republish the app.
8. Create a new poll, submit a response, view results, then restart the app to confirm the data persists.

This app starts MongoDB as a clean backend. It does not migrate existing SQLite pilot data into Atlas.

## Test The Deployed App

1. Open the deployed root URL:

```text
https://your-connect-app.example/
```

2. Request an organizer login code using the `APP_MAIN_OWNER_EMAIL` address.
3. Enter the code and open the **Create poll** tab.
4. Create a test poll.
5. Optionally test secondary organizer access by submitting a request from the root page with another email, then signing in as the main owner to approve it.
6. Share only the generated public `?respond=<token>` link with participants. Participants verify by email before submitting or editing availability.
7. Keep the generated private `?admin=<token>` organizer link as backup access.

Participants can respond through the effective expiry date. By default, that is the final proposed meeting date. If an earlier response deadline is set, that earlier date is used.

## SQLite Pilot Caveats

SQLite is suitable for local development and short proof-of-concept testing only. It is not durable production persistence on Connect Cloud because redeployments or restarts can remove app-local files.

Before production use:

- move persistence to a hosted database such as MongoDB Atlas, PostgreSQL, Supabase, Neon, Azure SQL, or another managed database;
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

Confirm that `APP_AUTH_SECRET` or `ORGANIZER_AUTH_SECRET` and all SMTP variables are set in the deployed content settings. Keep `ALLOW_DEV_AUTH_CODE_DISPLAY=false` on public deployments.

### MongoDB connection fails

Confirm that `DATABASE_BACKEND=mongodb`, `MONGODB_URI`, and `MONGODB_DATABASE` are set in the deployed content settings. Also confirm that the database user password in `MONGODB_URI` is current and that the Posit Connect Cloud outbound IP addresses are allowlisted in MongoDB Atlas.

### Poll data disappeared

This is a known limitation of SQLite proof-of-concept deployments. Export results before redeploying, or switch to the MongoDB Atlas backend before collecting important data.
