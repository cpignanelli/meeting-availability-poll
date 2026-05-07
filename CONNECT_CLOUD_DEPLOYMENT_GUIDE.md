# Deploy This App To GitHub And Posit Connect Cloud

This guide is written for a first-time deployment using:

- GitHub account: <https://github.com/cpignanelli>
- Posit Connect Cloud account: <https://connect.posit.cloud/cpignanelli1994>
- Current live app URL: <https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/>
- Local project folder: `/Users/chrispignanelli/Documents/New project 2`

The free Posit Connect Cloud plan requires a **public GitHub repository** for this workflow. Do not put real participant data, secrets, or the local SQLite database in GitHub.

## Current Live Pilot Settings

In Posit Connect Cloud, the app should have these environment variables:

```text
SQLITE_DB_PATH=data/app.sqlite
APP_BASE_URL=https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/
POLL_CREATION_SECRET=<your-private-creation-secret>
ORGANIZER_AUTH_SECRET=<a-different-private-random-secret>
```

Use the exact live URL above for `APP_BASE_URL`, including the trailing `/`.

Do not commit the real values of `POLL_CREATION_SECRET` or `ORGANIZER_AUTH_SECRET` to GitHub. If you used an example value from an earlier draft of this guide, change it in Posit Connect Cloud to a new private value before sharing the app.

Your private poll creation URL is:

```text
https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/?create=<your-private-creation-secret>
```

Share only the generated participant `?respond=<token>` links with colleagues. Keep both the creation URL and generated admin links private.

You can create multiple live booking polls at the same time. Each poll has its own public response link, private organizer link, and automatic link expiry after the final proposed meeting date. You can also set an earlier response deadline / link expiry date. The organizer portal is available at `?organizer=login` and lists polls tied to the organizer email used at creation. Save private organizer links as a backup.

The app stores proposed times internally in UTC, such as `2026-05-06T13:00:00Z`, but shows readable local meeting options with the selected IANA time zone, such as `Wed, May 6th, 9-10 AM EDT` and `America/Toronto`.

## What You Are Publishing

You are publishing the app code only. You are not publishing the local test database.

Commit these files/folders:

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

Never commit these:

```text
data/app.sqlite
.Renviron
.Rhistory
.RData
.DS_Store
BookingApp/
```

## Part 1: Confirm The App Works Locally

1. Open **RStudio**.
2. Open the project folder:

```text
/Users/chrispignanelli/Documents/New project 2
```

3. In RStudio, open the **Terminal** tab.
4. Run:

```sh
Rscript tests/testthat.R
```

5. You should see the tests finish without failures.
6. Generate or refresh the Connect Cloud dependency file:

```sh
Rscript scripts/write_connect_cloud_manifest.R
```

This updates `manifest.json`. Posit Connect Cloud needs this file to install the right R packages.

## Part 2: Create A Public GitHub Repository

1. Go to <https://github.com/cpignanelli>.
2. Click the green **New** button.
3. Repository name:

```text
meeting-availability-poll
```

4. Choose **Public**.
5. Do **not** add a README.
6. Do **not** add a `.gitignore`.
7. Do **not** add a license.
8. Click **Create repository**.
9. GitHub will show a page with setup commands. You only need the repository URL:

```text
https://github.com/cpignanelli/meeting-availability-poll.git
```

## Part 3: Connect RStudio Project To GitHub

In the RStudio **Terminal** tab, run:

```sh
git remote add origin https://github.com/cpignanelli/meeting-availability-poll.git
```

If you see an error that says `remote origin already exists`, run:

```sh
git remote -v
```

If the remote is not `https://github.com/cpignanelli/meeting-availability-poll.git`, fix it:

```sh
git remote set-url origin https://github.com/cpignanelli/meeting-availability-poll.git
```

## Part 4: Commit The App In RStudio

1. In RStudio, click the **Git** tab.
2. Click **Diff** or **Commit**.
3. Select the checkbox beside the files you want to stage.
4. You should stage the app files and folders, including:

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

5. Make sure you do **not** stage:

```text
data/app.sqlite
.DS_Store
BookingApp/
```

These should be ignored by Git.

6. In the commit message box, enter:

```text
Initial meeting availability poll Shiny app
```

7. Click **Commit**.

## Part 5: Push To GitHub

In RStudio, click **Push** in the Git pane.

If RStudio asks you to authenticate, follow the GitHub login/browser prompts.

If you prefer the Terminal, run:

```sh
git branch -M main
git push -u origin main
```

After the first push, future updates are simpler:

```sh
git status --short --ignored
git add app.R manifest.json renv.lock R www data/.gitkeep scripts tests README.md CONNECT_CLOUD_DEPLOYMENT_GUIDE.md .gitignore .rscignore .Renviron.example
git commit -m "Update meeting availability poll app"
git push
```

After pushing, open:

```text
https://github.com/cpignanelli/meeting-availability-poll
```

Confirm that `app.R`, `manifest.json`, `R/`, `www/`, and `data/.gitkeep` are visible.

## Part 6: Publish From GitHub To Posit Connect Cloud

1. Go to <https://connect.posit.cloud/cpignanelli1994>.
2. Click **Publish**.
3. If prompted, authorize/install the Posit GitHub App.
4. Choose **Shiny**.
5. Select the repository:

```text
cpignanelli/meeting-availability-poll
```

6. Select the branch:

```text
main
```

7. Select the primary file:

```text
app.R
```

8. Confirm that Connect Cloud sees `manifest.json`.
9. Open **Advanced settings**.

## Part 7: Add Secret / Environment Variables

In the Connect Cloud publish screen or content settings, add these variables.

Use this exactly:

```text
SQLITE_DB_PATH=data/app.sqlite
```

Choose a private creation secret, but do not put the real value in GitHub:

```text
POLL_CREATION_SECRET=<your-private-creation-secret>
```

This secret keeps random public visitors from creating polls. Do not put this value in GitHub.

Add a separate secret for organizer email-code login:

```text
ORGANIZER_AUTH_SECRET=<a-different-private-random-secret>
```

To use `?organizer=login` on Connect Cloud, configure SMTP variables:

```text
SMTP_HOST=<your-smtp-host>
SMTP_PORT=587
SMTP_USERNAME=<your-smtp-username>
SMTP_PASSWORD=<your-smtp-password>
SMTP_FROM=<verified-sender@example.org>
SMTP_USE_SSL=false
ALLOW_DEV_AUTH_CODE_DISPLAY=false
```

For local testing only, you can leave SMTP blank and set `ALLOW_DEV_AUTH_CODE_DISPLAY=true`; do not use that setting on a public app.

If Connect Cloud already shows the final app URL, also add:

```text
APP_BASE_URL=https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/
```

If you do not know the final app URL yet, skip `APP_BASE_URL` for now. You will add it after the first publish.

## Part 8: Publish The App

1. Click **Publish**.
2. Wait for the build logs to finish.
3. If the build fails, open the logs and look for the first red error.
4. If it succeeds, copy the app URL.

The URL may look like:

```text
https://connect.posit.cloud/cpignanelli1994/content/...
```

## Part 9: Add APP_BASE_URL After Publishing

The app needs `APP_BASE_URL` so the generated participant/admin links use the public Connect Cloud URL.

1. Open the app in Posit Connect Cloud.
2. Open the app/content settings.
3. Find **Advanced settings** or **Environment variables**.
4. Add:

```text
APP_BASE_URL=https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/
```

Use the exact app URL above, including the trailing `/`.

5. Save.
6. Republish or restart the content.

## Part 10: Create Your First Test Poll

Use the private creation URL:

```text
https://cpignanelli1994-meeting-availability-poll.share.connect.posit.cloud/?create=<your-private-creation-secret>
```

Replace the secret with whatever value you set in `POLL_CREATION_SECRET`.

Create a test poll. The app will show two links:

- **Public response link**: send this to your colleague.
- **Private organizer link**: keep this for yourself.

Participants can respond through the effective expiry date. By default, that is the final proposed meeting date. If you set an earlier response deadline / link expiry, the earlier date is used. Starting the next day, the participant link will show a closed message with your organizer name and email. From the private organizer dashboard, you can close a non-finalized response link, reopen an expired or closed link with a new expiry date, or reopen it using the latest proposed meeting date as the expiry.

## Part 11: Free Plan Limits

For the free Connect Cloud plan:

- The GitHub repository must be public.
- The deployed app is publicly accessible.
- You cannot use private sharing controls on the Free plan.
- Use fake or low-risk test data only.
- SQLite is only for a demo. It is not durable production storage on Connect Cloud.
- Republish/restart events can remove the SQLite data.
- Save each private organizer link. Without an organizer login dashboard, that link is how you manage results and reopen/close the response link.

For real production use, move to a paid plan and a hosted database such as PostgreSQL, Supabase, Neon, or Azure SQL.

## Troubleshooting

### The repository does not show up in Connect Cloud

Check that:

- The GitHub repository is public.
- The Posit GitHub App is installed/authorized.
- The app files were pushed to GitHub.
- `manifest.json` exists in the repository.

### Connect Cloud says dependency file is missing

Run this locally:

```sh
Rscript scripts/write_connect_cloud_manifest.R
git add manifest.json
git commit -m "Update Connect Cloud manifest"
git push
```

Then republish in Connect Cloud.

### The app URL works, but poll links point to localhost

Set `APP_BASE_URL` in Connect Cloud environment variables to the public app URL, then restart/republish.

### The app says poll creation is restricted

That is expected when `POLL_CREATION_SECRET` is set. Add `?create=<your-secret>` to the URL.

### Poll data disappeared

That is a known limitation of using SQLite on Connect Cloud Free. Export results before republishing. For real use, migrate to a hosted database.
