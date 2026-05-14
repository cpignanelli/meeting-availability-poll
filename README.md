# Meeting Availability Poll

A modular R/Shiny application for creating manual meeting availability polls across organizations. It is similar in concept to Doodle, but it does not connect to Outlook, Google Calendar, Microsoft Graph, or any participant calendar. Organizers manually propose candidate times, participants submit availability, and the organizer reviews ranked results through a private link.

For a generic GitHub and Posit Connect Cloud deployment walkthrough, see `CONNECT_CLOUD_DEPLOYMENT_GUIDE.md`.

Keep real secret values only in Posit Connect Cloud environment variables. Do not commit them to GitHub.

## User workflow

1. The organizer opens the app root URL, signs in with email and a 6-digit code, then creates a poll with meeting details, time zone, optional location details, a Doodle-style proposed-time grid, and an optional earlier response deadline.
   - The configured main owner can approve or deny secondary organizer access requests.
   - Approved secondary organizers can create and manage only the polls tied to their own organizer email.
2. The app generates two links:
   - Public response link: `?respond=<token>`
   - Private organizer link: `?admin=<token>`
3. Participants open the public link, verify their email with a 6-digit code, submit name, availability for each option, and optional organizer-only comments through a compact Doodle-like availability board.
4. The organizer reviews a decision-focused dashboard through the signed-in workspace or a private admin link.
5. The organizer selects the final time, generates copy-ready final email text, optionally downloads an `.ics` file, and closes the poll.

Organizers can create multiple live booking polls at the same time. Each poll has its own public response link and private organizer link. The organizer portal lists polls tied to the organizer email used at creation. Private admin links still work and should still be saved.

## User experience

The app uses a minimal white interface with near-black text and restrained cardinal red accents. The organizer creation page uses a two-column meeting details form, optional browser-only organizer name/email memory, duration pills, and a week calendar for selecting proposed times.

The public participant page is intentionally simple:

- meeting summary first;
- email-code access before response entry;
- a compact availability board showing only the dates and times proposed by the organizer;
- verified participants can see other participants' names and availability, but not emails or comments;
- explicit labels for "Preferred", "Available", and "Unavailable";
- clear messaging that final meeting confirmation will follow from the organizer;
- no public access to admin results, expected participant lists, private links, or organizer-only details.

The organizer dashboard is optimized for deciding quickly: the overview shows a compact poll header, best-ranked option, response progress, response-link status, shareable participant link, ranked options, availability board, exports, and finalization controls. The app root URL opens the organizer workspace with "My polls" and "Create poll" tabs. The main owner also sees "Access requests" and "Approved owners" tabs.

## Date, time, and time zones

The app stores proposed meeting timestamps internally in UTC ISO 8601 form, such as `2026-05-06T13:00:00Z`. User-facing screens use readable meeting labels with the selected IANA time zone, for example:

```text
Wed, May 6th, 9-10 AM EDT
America/Toronto
```

Participant response pages and organizer dashboards detect the browser's IANA time zone and show proposed times in that time zone by default, with a manual "Times shown in" selector. The app does not request GPS/browser location permission, and viewer time zones are used only for display; stored timestamps and expiry checks remain anchored to UTC and the poll time zone.

## Response link expiry and reopening

Each poll automatically expires after the final proposed meeting date. Organizers can optionally set an earlier response deadline / link expiry date. If the chosen date is later than the final proposed meeting date, the app caps the effective expiry at the final proposed meeting date.

Participants can respond through the effective expiry date in the poll's time zone. Starting the next day, the response link shows a closed message with the organizer's name and email so the participant knows whom to contact.

From the private organizer dashboard, non-finalized polls can be:

- closed manually;
- reopened with a new expiry date;
- reopened using the latest proposed meeting date as the expiry.

Finalized polls cannot be reopened in this version. Create a new poll if a finalized meeting needs to be rescheduled.

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
  "openssl", "digest", "htmltools", "testthat", "rsconnect",
  "curl", "blastula", "jsonlite", "mongolite"
))
```

`blastula` is used when available for SMTP login-code email and response notifications. The app can also send through `curl` when SMTP is configured. For local development without SMTP, set `ALLOW_DEV_AUTH_CODE_DISPLAY=true` to show organizer login codes on screen.

`mongolite` is required only when `DATABASE_BACKEND=mongodb`, but it is included in the project dependencies so hosted deployments can switch from local SQLite to MongoDB Atlas without code changes.

Owner access request notifications and participant response notifications use the same SMTP settings. On public deployments, configure SMTP so organizers receive new-response alerts and participants receive links to return and edit availability.

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

## MongoDB Atlas backend for live pilot use

SQLite remains the default because it is simple and works locally. For a live Posit Connect pilot where data should survive app restarts and redeployments, set the app to use MongoDB Atlas with environment variables:

```text
DATABASE_BACKEND=mongodb
MONGODB_URI=<mongodb+srv connection string>
MONGODB_DATABASE=meeting_poll
```

`MONGODB_URI` contains the database username and password, so store it only in `.Renviron` locally or in deployment secret variables. Do not commit it to Git.

MongoDB setup checklist:

1. Create a MongoDB Atlas Free cluster.
2. Create a database user with read/write access to the app database.
3. Add the Posit Connect Cloud outbound IP addresses to the MongoDB Atlas IP Access List.
4. Save `DATABASE_BACKEND`, `MONGODB_URI`, and `MONGODB_DATABASE` in the deployed app variables.
5. Republish or restart the app.
6. Create a new test poll and verify that responses and dashboard results persist after restart.

This integration starts with an empty MongoDB database. It does not migrate existing pilot data from `data/app.sqlite`.

## Running locally

From the project root:

```r
shiny::runApp()
```

Generated links use query-string routing:

- `?respond=<response_token>` for participants
- `?admin=<admin_token>` for the organizer
- `?organizer=login` for the email-code organizer portal
- `?organizer=login&poll=<response_token>` for organizer email notifications that open a specific poll after sign-in
- `?create=<POLL_CREATION_SECRET>` as a legacy/dev fallback creation route when `POLL_CREATION_SECRET` is set

For link generation outside RStudio, set `APP_BASE_URL` in `.Renviron`:

```text
APP_BASE_URL=https://your-app.example.com
DATABASE_BACKEND=sqlite
SQLITE_DB_PATH=data/app.sqlite
APP_AUTH_SECRET=replace-with-a-long-random-value
ORGANIZER_AUTH_SECRET=replace-with-a-long-random-value
TRUSTED_SESSION_MINUTES=10
```

Use `.Renviron.example` as a template. Do not commit `.Renviron`.

The app root URL `/` opens the organizer email-code workspace. Poll creation in the normal workflow requires successful organizer login. A short-lived signed browser session avoids repeated codes after quick refreshes or browser reopenings within `TRUSTED_SESSION_MINUTES`. The buffer is specific to the same browser, device, app URL, and stable `APP_AUTH_SECRET`; private browsing, cleared site data, a changed app URL, or rotating the secret will require a new code. The `?create=` URL remains available only as a hidden fallback/dev route.

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
- Anyone with the public response link can submit a response while the link is open.
- Anyone with the private organizer link can view results.
- Use fake or low-risk test data unless you deploy with production controls.

## Database architecture

The local proof of concept uses SQLite through `DBI` and `RSQLite`. Runtime connections are created through `get_db_connection()`, which uses `pool` when available. Schema creation is idempotent through `initialize_database()`.

For hosted pilot persistence, `DATABASE_BACKEND=mongodb` switches the query layer to MongoDB Atlas through `mongolite`. The UI modules still call the same functions in `R/db/db_queries.R`; backend-specific behavior is contained in `R/db/db_connect.R`, `R/db/db_schema.R`, and `R/db/db_mongo.R`.

Core tables:

- `polls`
- `poll_options`
- `expected_participants`
- `participants`
- `responses`
- `finalized_meetings`
- `audit_log`
- `organizer_login_codes`
- `owner_access_requests`
- `approved_owners`

MongoDB uses collections with the same logical names plus a `counters` collection for numeric IDs used by the existing Shiny modules.

All Shiny modules call the query layer in `R/db/db_queries.R`; raw SQL is not scattered through UI modules. SQLite writes use parameterized queries and transaction wrappers. MongoDB writes go through the adapter in `R/db/db_mongo.R` and keep the same module-facing query API.

## Security and privacy

Treat all names, emails, comments, and availability data as sensitive personal information.

Implemented in this proof of concept:

- Random 256-bit hex tokens for public and private links.
- Admin tokens are stored as SHA-256 hashes, not raw tokens.
- Organizer portal magic codes are stored only as hashes, expire after 10 minutes, and are limited to 5 verification attempts.
- Organizer and participant email-code sign-ins issue signed browser-local session tokens for the configured `TRUSTED_SESSION_MINUTES` window. These tokens are revalidated by the server on restore and are cleared on sign-out, expiry, tampering, revoked organizer access, or changing participant email.
- Organizer workspace access is restricted to the configured main owner and approved secondary owners.
- Secondary owner access requests require first name, last name, email, email verification, and main-owner approval.
- Response notification emails are sent after saved participant responses when SMTP is configured. Organizer notification links require organizer sign-in and do not expose private admin tokens.
- Public response links do not expose results.
- Private organizer links or an email-code organizer portal login are required for dashboard access.
- Closed or expired response links show organizer contact details so participants can ask whether the link can be reopened.
- User input is validated, trimmed, length-limited, and escaped when rendered.
- Database writes use parameterized DBI queries.
- Audit logs avoid personal information.
- Local SQLite files and secrets are ignored by Git.
- MongoDB connection strings are read only from environment variables and should be stored as deployment secrets.
- App errors shown to users avoid database paths, stack traces, and secrets.

Production hardening recommendations:

- Add reverse-proxy or platform-level rate limiting.
- Use a production SMTP account for organizer login codes and monitor failed login attempts without storing raw codes.
- Add organization authentication for organizer/admin access if this grows beyond a lightweight email-code portal.
- Add role-based access control if multiple organizers manage polls.
- Move owner approvals to durable hosted storage before production use.
- Use a hosted database with encrypted storage and managed backups.
- Define a retention policy, such as deleting or anonymizing poll data after 30 to 90 days.
- Add monitoring for failed admin token attempts without logging raw tokens or personal information.

## Deployment notes

The app can be deployed later to Posit Connect or shinyapps.io. For production, do not rely on local writable files for persistence. Use a hosted database such as MongoDB Atlas, PostgreSQL, Supabase, Neon, Azure SQL, or another managed database service.

Recommended deployment approach:

- Keep SQLite for local development only.
- Set production secrets through environment variables, not code.
- Use `DATABASE_BACKEND=mongodb`, `MONGODB_URI`, and `MONGODB_DATABASE` for the MongoDB Atlas adapter.
- Review platform-specific file persistence behavior before deploying.

## Posit Connect pilot deployment

This section is for testing the app in your own Posit Connect account with low-risk pilot data. SQLite can be used as a short-lived proof-of-concept database, but MongoDB Atlas is the better free hosted option when you need data to persist across app restarts and redeployments. Posit Connect can let interactive apps write to their working directory, but that directory is not appropriate for durable persistence because redeployments replace the bundle directory and multiple app processes can collide on local file writes.

### Posit Connect Cloud Free note

If you are using **Posit Connect Cloud Free**, use the GitHub publishing workflow instead of the API-key workflow below. The Free plan is intended for public applications and documents from public GitHub repositories. You do not need a custom domain for this. The Domains page only controls whether you can connect your own URL.

For this app, the Free plan is acceptable for a short public proof of concept with non-sensitive test data. It is not appropriate for production meeting coordination with real personal information because:

- public access is enabled on Free plans;
- files written while the app runs are not durable persistent storage;
- the SQLite proof-of-concept database can disappear when the app restarts or is republished;
- anyone who can access the app URL can reach the app, so poll creation should require organizer email-code login.

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
```

4. In Posit Connect Cloud, click **Publish**.
5. Choose **Shiny**.
6. Select or paste your public GitHub repository.
7. Select the branch.
8. Select `app.R` as the primary file.
9. In Advanced settings, add secret/environment variables:

```text
APP_MAIN_OWNER_EMAIL=owner@example.org
APP_AUTH_SECRET=<a-different-private-random-secret>
ORGANIZER_AUTH_SECRET=<a-different-private-random-secret>
TRUSTED_SESSION_MINUTES=10
SMTP_HOST=smtp.example.org
SMTP_PORT=587
SMTP_USERNAME=you@example.org
SMTP_PASSWORD=<your-smtp-password>
SMTP_FROM=you@example.org
SMTP_USE_SSL=false
ALLOW_DEV_AUTH_CODE_DISPLAY=false
```

For durable pilot persistence with MongoDB Atlas, also add:

```text
DATABASE_BACKEND=mongodb
MONGODB_URI=<mongodb+srv connection string>
MONGODB_DATABASE=meeting_poll
```

For a short SQLite-only pilot, use this instead of the MongoDB variables:

```text
DATABASE_BACKEND=sqlite
SQLITE_DB_PATH=data/app.sqlite
```

If Connect Cloud shows the final public URL before publish, also set:

```text
APP_BASE_URL=https://your-connect-app.example/
```

If you do not know the URL yet, publish first, copy the deployed URL, then add `APP_BASE_URL` in the content settings and republish/restart.

Optional legacy fallback:

```text
POLL_CREATION_SECRET=<your-private-creation-secret>
```

For local testing only, you may leave SMTP blank and set `ALLOW_DEV_AUTH_CODE_DISPLAY=true` so the app displays the login code after you request it.

10. Create polls by opening the app root URL, signing in, and using the **Create poll** tab:

```text
https://your-connect-app.example/
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
APP_MAIN_OWNER_EMAIL=owner@example.org
APP_AUTH_SECRET=replace-with-a-long-random-value
ORGANIZER_AUTH_SECRET=replace-with-a-long-random-value
TRUSTED_SESSION_MINUTES=10
SMTP_HOST=smtp.example.org
SMTP_PORT=587
SMTP_USERNAME=you@example.org
SMTP_PASSWORD=replace-with-smtp-password
SMTP_FROM=you@example.org
SMTP_USE_SSL=false
ALLOW_DEV_AUTH_CODE_DISPLAY=false
```

For MongoDB Atlas hosted pilot persistence, add:

```text
DATABASE_BACKEND=mongodb
MONGODB_URI=<mongodb+srv connection string>
MONGODB_DATABASE=meeting_poll
```

For a SQLite-only proof of concept, add:

```text
DATABASE_BACKEND=sqlite
SQLITE_DB_PATH=data/app.sqlite
```

Optional but recommended after the final URL is known:

```text
APP_BASE_URL=https://connect.example.org/your-app-path/
```

4. Restart the app/content after changing environment variables.
5. For the SQLite pilot only, keep the app to one running process if your Connect server exposes process scaling settings. This avoids concurrent SQLite writes from multiple R processes. MongoDB Atlas does not need that SQLite-specific constraint.

### 5. Create a test poll

Open the deployed root URL, request a login code, enter the code, then use the **Create poll** tab:

```text
https://connect.example.org/your-app-path/
```

Create the poll, then share only the generated public response link with your colleague. Keep the generated private organizer link as a backup.

### 6. Redeploying during the pilot

Redeploy with:

```r
source("scripts/deploy_posit_connect.R")
```

Important: a redeployment can remove files written to the app working directory, including the SQLite proof-of-concept database. Export results before redeploying, or use the MongoDB Atlas backend before collecting important data.

### 7. Production path after the pilot

Before production use:

- Use a hosted database such as MongoDB Atlas, PostgreSQL, Supabase, Neon, Azure SQL, or another managed database.
- Keep secrets in Posit Connect environment variables.
- Keep poll creation behind authentication or a stronger admin workflow.
- Define retention/deletion rules for personal information.
- Add platform-level rate limiting or other abuse controls.

Files to commit:

- Source code under `R/`, `app.R`, scripts, tests, README, `.Renviron.example`, `.gitignore`, `.rscignore`, `renv.lock`, `data/.gitkeep`, and files under `www/`.
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
- Database backend selection
- Local SQLite initialization
- Poll creation and token lookup
- Token generation and hashing
- Participant email-code access, response submission, and update
- Trusted browser session token validation
- Participant-visible response privacy
- Viewer-local time-zone rendering
- Availability scoring and ranking
- Final meeting selection
- Input validation

## Known limitations

- SQLite is suitable for local proof of concept only.
- Organizer access uses email-code login plus owner approval; private admin links remain as backup access.
- SMTP email is required for public organizer and participant code delivery.
- No calendar integrations are implemented.
- Participant response editing requires email-code verification for the poll.
- Rate limiting is documented but not implemented in app code.

## Future enhancements

- Email reminders.
- Automated final email notifications.
- Outlook or Google calendar invite generation, if explicitly configured by the app owner.
- Poll expiration automation.
- Longer-lived participant accounts.
- Admin authentication with organization login.
- Role-based access control.
- Automatic deletion or anonymization after a retention period.
- Optional weighted scoring configuration.
- Migration from local SQLite to a hosted production database.
