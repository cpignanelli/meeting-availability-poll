# Meeting Availability Poll

A modular R/Shiny application for creating manual meeting availability polls across organizations. It is similar in concept to Doodle, but it does not connect to Outlook, Google Calendar, Microsoft Graph, or any participant calendar. Organizers manually propose candidate times, participants submit availability, and the organizer reviews ranked results through a private link.

For a beginner-friendly GitHub and Posit Connect Cloud deployment walkthrough, see `CONNECT_CLOUD_DEPLOYMENT_GUIDE.md`.

## User workflow

1. The organizer creates a poll with meeting details, duration, time zone, proposed times, optional location details, optional deadline, and optional expected participants.
2. The app generates two links:
   - Public response link: `?respond=<token>`
   - Private organizer link: `?admin=<token>`
3. Participants submit name, email, organization, availability for each option, and optional comments.
4. The organizer reviews summary cards, ranked slots, a heatmap, responses, missing expected participants, and CSV exports.
5. The organizer selects the final time, generates copy-ready final email text, optionally downloads an `.ics` file, and closes the poll.

## Required R packages

This project is intended to use `renv` for reproducible dependency management. If a lockfile is present, run:

```r
install.packages("renv")
renv::restore()
```

For a simple manual setup, install the required packages:

```r
install.packages(c(
  "shiny", "bslib", "DBI", "RSQLite", "pool", "DT",
  "openssl", "digest", "htmltools", "testthat", "rsconnect"
))
```

## Local database setup for proof of concept

1. Install required R packages.
2. Open the project in RStudio.
3. Run `renv::restore()` if `renv` is used, or install the listed packages manually.
4. Run the app with:

```r
shiny::runApp()
```

5. The app automatically creates `data/app.sqlite` the first time it launches.
6. To manually initialize the database, run:

```r
source("scripts/init_local_db.R")
```

7. To reset the local database, close the app, open `scripts/reset_local_db.R`, set `CONFIRM_RESET <- TRUE`, then run the script. This deletes only `data/app.sqlite` and recreates an empty schema.
8. To inspect the database, use DB Browser for SQLite or R:

```r
con <- DBI::dbConnect(RSQLite::SQLite(), "data/app.sqlite")
DBI::dbListTables(con)
DBI::dbReadTable(con, "polls")
DBI::dbDisconnect(con)
```

9. SQLite is intended for local proof-of-concept use.
10. A hosted database is recommended for production deployment.

## Running locally

From the project root:

```r
shiny::runApp()
```

The default create-poll page is `/`. Generated links use query-string routing:

- `?respond=<response_token>` for participants
- `?admin=<admin_token>` for the organizer

For link generation outside RStudio, set `APP_BASE_URL` in `.Renviron`:

```text
APP_BASE_URL=https://your-app.example.com
SQLITE_DB_PATH=data/app.sqlite
```

Use `.Renviron.example` as a template. Do not commit `.Renviron`.

## Sharing a live proof of concept

For a quick live test with one colleague, keep the app running on your computer and expose it with either a trusted tunnel or your local network. This is appropriate for test data only. Do not use this approach for production data.

Recommended quick-test workflow with a tunnel:

1. Start a tunnel to local port `3838` with a tool approved by your organization, such as Cloudflare Tunnel or ngrok.
2. Copy the public HTTPS URL from the tunnel.
3. In R, set the URL before launching the app:

```r
Sys.setenv(APP_BASE_URL = "https://your-public-tunnel-url.example")
Sys.setenv(APP_HOST = "127.0.0.1")
Sys.setenv(APP_PORT = "3838")
source("scripts/run_live_local.R")
```

4. Open the public tunnel URL yourself, create a test poll, and share only the generated public response link with your colleague.
5. Keep your R session, tunnel, and computer running while your colleague responds.

Local network workflow:

1. Confirm your colleague is on the same trusted network or VPN.
2. Find your computer's local IP address.
3. Start the app with:

```r
Sys.setenv(APP_BASE_URL = "http://YOUR_LOCAL_IP:3838")
Sys.setenv(APP_HOST = "0.0.0.0")
Sys.setenv(APP_PORT = "3838")
source("scripts/run_live_local.R")
```

4. Share the generated participant link.

Live proof-of-concept caveats:

- The data is stored on your computer in `data/app.sqlite`.
- If your R session stops, the app is offline.
- Anyone with the public response link can submit a response.
- Anyone with the private organizer link can view results.
- Use fake or low-risk test data unless you deploy with production controls.

## Database architecture

The local proof of concept uses SQLite through `DBI` and `RSQLite`. Runtime connections are created through `get_db_connection()`, which uses `pool` when available. Schema creation is idempotent through `initialize_database()`.

Core tables:

- `polls`
- `poll_options`
- `expected_participants`
- `participants`
- `responses`
- `finalized_meetings`
- `audit_log`

All Shiny modules call the query layer in `R/db/db_queries.R`; raw SQL is not scattered through UI modules. Writes use parameterized queries and transaction wrappers.

## Security and privacy

Treat all names, emails, organizations, comments, and availability data as sensitive personal information.

Implemented in this proof of concept:

- Random 256-bit hex tokens for public and private links.
- Admin tokens are stored as SHA-256 hashes, not raw tokens.
- Public response links do not expose results.
- Private organizer links are required for dashboard access.
- User input is validated, trimmed, length-limited, and escaped when rendered.
- Database writes use parameterized DBI queries.
- Audit logs avoid personal information.
- Local SQLite files and secrets are ignored by Git.
- App errors shown to users avoid database paths, stack traces, and secrets.

Production hardening recommendations:

- Add reverse-proxy or platform-level rate limiting.
- Add organization authentication for organizer/admin access.
- Add role-based access control if multiple organizers manage polls.
- Use a hosted database with encrypted storage and managed backups.
- Define a retention policy, such as deleting or anonymizing poll data after 30 to 90 days.
- Add monitoring for failed admin token attempts without logging raw tokens or personal information.

## Deployment notes

The app can be deployed later to Posit Connect or shinyapps.io. For production, do not rely on local writable files for persistence. Use a hosted database such as PostgreSQL, Supabase, Neon, Azure SQL, or another managed database service.

Recommended deployment approach:

- Keep SQLite for local development only.
- Set production secrets through environment variables, not code.
- Move the database implementation behind the existing DBI query layer.
- Prefer `DATABASE_URL` or explicit `DB_HOST`, `DB_NAME`, `DB_USER`, `DB_PASSWORD`, and `DB_PORT` values for hosted databases.
- Review platform-specific file persistence behavior before deploying.

## Posit Connect pilot deployment

This section is for testing the app in your own Posit Connect account with low-risk pilot data. It uses SQLite only as a short-lived proof-of-concept database. Posit Connect allows interactive apps to write to their working directory, but that directory is not appropriate for durable production storage because redeployments replace the bundle directory and multiple app processes can collide on local file writes.

### Posit Connect Cloud Free note

If you are using **Posit Connect Cloud Free**, use the GitHub publishing workflow instead of the API-key workflow below. The Free plan is intended for public applications and documents from public GitHub repositories. You do not need a custom domain for this. The Domains page only controls whether you can connect your own URL.

For this app, the Free plan is acceptable for a short public proof of concept with non-sensitive test data. It is not appropriate for production meeting coordination with real personal information because:

- public access is enabled on Free plans;
- files written while the app runs are not durable persistent storage;
- the SQLite proof-of-concept database can disappear when the app restarts or is republished;
- anyone who can access the app URL can reach the app, so poll creation should use `POLL_CREATION_SECRET`.

#### Connect Cloud Free GitHub workflow

1. Make sure the local SQLite database is not committed:

```sh
git status --short --ignored
```

You should see `data/app.sqlite` ignored.

2. Generate the required `manifest.json` for the R Shiny app:

```r
source("scripts/write_connect_cloud_manifest.R")
```

3. Commit and push the app to a **public GitHub repository**. Include:

```text
app.R
manifest.json
R/
www/
data/.gitkeep
.gitignore
.rscignore
README.md
```

Do not commit:

```text
data/app.sqlite
.Renviron
.Rhistory
.RData
BookingApp/
```

4. In Posit Connect Cloud, click **Publish**.
5. Choose **Shiny**.
6. Select or paste your public GitHub repository.
7. Select the branch.
8. Select `app.R` as the primary file.
9. In Advanced settings, add secret/environment variables:

```text
SQLITE_DB_PATH=data/app.sqlite
POLL_CREATION_SECRET=choose-a-long-random-value
```

If Connect Cloud shows the final public URL before publish, also set:

```text
APP_BASE_URL=https://your-connect-cloud-content-url/
```

If you do not know the URL yet, publish first, copy the deployed URL, then add `APP_BASE_URL` in the content settings and republish/restart.

10. Create polls with:

```text
https://your-connect-cloud-content-url/?create=choose-a-long-random-value
```

11. Share only the generated `?respond=<token>` participant link with your colleague. Keep the generated `?admin=<token>` organizer link private.

#### Alternative: Connect Cloud from IDE

If you use Positron or VS Code, Connect Cloud supports the Posit Publisher extension. If you use RStudio or an R session, Connect Cloud can also be connected with:

```r
install.packages("rsconnect")
rsconnect::connectCloudUser()
```

This opens a browser authorization flow rather than asking you to manually create an API key. The GitHub workflow is still the safest path for the Free plan because Free is designed for public GitHub repository publishing.

### 1. Prepare the project locally

Run the tests before deploying:

```r
Rscript tests/testthat.R
```

Install deployment tooling if needed:

```r
install.packages("rsconnect")
```

### 2. Connect your R session to Posit Connect

In Posit Connect, create an API key from your user/account settings. Then run this once, replacing the server URL, account name, and API key:

```r
rsconnect::addServer(
  "https://connect.example.org",
  name = "my-connect"
)

rsconnect::connectApiUser(
  server = "my-connect",
  account = "your-connect-username",
  apiKey = "paste-your-api-key-here"
)
```

Do not commit API keys or place them in `.Renviron`.

### 3. Deploy the app

The deployment script bundles only the app source files. It excludes `data/app.sqlite`, `.Renviron`, local RStudio files, tests, and local scratch folders.

```r
Sys.setenv(CONNECT_SERVER_NAME = "my-connect")
Sys.setenv(CONNECT_ACCOUNT = "your-connect-username")
source("scripts/deploy_posit_connect.R")
```

After deployment, Posit Connect opens the content page.

### 4. Configure content settings in Posit Connect

In the deployed content settings:

1. Open the **Access** or **Sharing** settings.
2. Choose who can view the app:
   - For an internal pilot, use specific users/groups or all logged-in users.
   - For an external colleague without a Connect login, use **Anyone - no login required** only if your Connect license/server allows public interactive content.
3. Open the **Advanced** settings and add environment variables:

```text
SQLITE_DB_PATH=data/app.sqlite
POLL_CREATION_SECRET=replace-with-a-long-random-value
```

Optional but recommended after the final URL is known:

```text
APP_BASE_URL=https://connect.example.org/your-app-path/
```

4. Restart the app/content after changing environment variables.
5. For the SQLite pilot only, keep the app to one running process if your Connect server exposes process scaling settings. This avoids concurrent SQLite writes from multiple R processes.

### 5. Create a test poll

If `POLL_CREATION_SECRET` is set, the root app URL is locked. Create polls using:

```text
https://connect.example.org/your-app-path/?create=replace-with-a-long-random-value
```

Create the poll, then share only the generated public response link with your colleague. Keep the generated private organizer link to yourself.

### 6. Redeploying during the pilot

Redeploy with:

```r
source("scripts/deploy_posit_connect.R")
```

Important: a redeployment can remove files written to the app working directory, including the SQLite proof-of-concept database. Export results before redeploying, or move to a hosted database before collecting important data.

### 7. Production path after the pilot

Before production use:

- Replace SQLite with a hosted database such as PostgreSQL, Supabase, Neon, Azure SQL, or another managed database.
- Keep secrets in Posit Connect environment variables.
- Keep poll creation behind authentication or a stronger admin workflow.
- Define retention/deletion rules for personal information.
- Add platform-level rate limiting or other abuse controls.

Files to commit:

- Source code under `R/`, `app.R`, scripts, tests, README, `.Renviron.example`, `.gitignore`, `.rscignore`, `renv.lock`, `data/.gitkeep`, and `www/custom.css`.
- After your first Posit Connect deployment, the generated `rsconnect/` deployment record can also be committed; it identifies the Connect server/content target and does not contain private secrets.

Files not to commit:

- `data/app.sqlite`
- `data/*.sqlite-journal`
- `data/*.db`
- `.Renviron`
- `.Rhistory`
- `.RData`
- Real user-submitted data

## Testing

Run:

```r
Rscript tests/testthat.R
```

Current tests cover:

- Database schema creation
- Local SQLite initialization
- Poll creation and token lookup
- Token generation and hashing
- Participant response submission and update
- Availability scoring and ranking
- Final meeting selection
- Input validation

## Known limitations

- SQLite is suitable for local proof of concept only.
- Admin links are the only organizer authentication mechanism in v1.
- No automated emails are sent.
- No calendar integrations are implemented.
- Participant response edit links are not implemented; resubmission by the same email replaces the previous response.
- Rate limiting is documented but not implemented in app code.

## Future enhancements

- Email reminders.
- Automated final email notifications.
- Outlook or Google calendar invite generation, if explicitly configured by the app owner.
- Poll expiration automation.
- Participant response edit links.
- Admin authentication with organization login.
- Role-based access control.
- Automatic deletion or anonymization after a retention period.
- Optional weighted scoring configuration.
- Migration from local SQLite to a hosted production database.
