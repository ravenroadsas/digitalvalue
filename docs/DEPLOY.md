# Deploying Digital Value on Posit Connect

This guide takes the app from the repository to a production deployment on Posit Connect. It covers:

* a shared folder for the database and its backups;
* the configuration and the superusers, stored as pins;
* the content settings and environment variables.

**Checklist**

1. [Prerequisites](#1-prerequisites)
2. [Shared database folder](#2-shared-database-folder)
3. [Configuration pins](#3-configuration-pins)
4. [Roles table (`dv_roles`)](#4-roles-table-dv_roles)
5. [Deploy the app](#5-deploy-the-app)
6. [Content settings](#6-content-settings)
7. [Environment variables](#7-environment-variables)
8. [Go-live checks](#8-go-live-checks)
9. [Operations: redeploys, backups, restore](#9-operations)
10. [Further recommendations](#10-further-recommendations)

---

## 1. Prerequisites

| Item | Why |
|---|---|
| A **service account** on Connect with the *publisher* role, e.g. `svc-digitalvalue` | It owns the app and the pins, so the deployment does not depend on one person's account. |
| An **API key** for the service account (Connect → your name → API Keys) | Used to deploy and to publish the pins from your computer. |
| **R** on Connect at the version you develop with (≥ 4.1) | Connect picks the closest R version available. |
| A **Posit Package Manager** repository (or CRAN binaries) set in Connect | `duckdb` takes a long time to compile from source. Binaries make deployments fast. |
| `rsconnect` (≥ 1.0) and `pins` installed locally | Deployment and pin publishing. |
| A **shared folder** on the Connect server, see below | Holds the DuckDB file and its backups. |

---

## 2. Shared database folder

The database is one DuckDB file (`DV_DB_PATH`). It must live **outside the content bundle**: every deployment creates a new bundle directory, so a file stored next to `app.R` would be lost at the next deploy.

### 2.1 Create the folder (Connect server administrator)

```bash
# on the Connect server (every node, if there are several; see 2.3)
sudo mkdir -p /data/digitalvalue/backups
sudo chown -R rstudio-connect:rstudio-connect /data/digitalvalue   # the RunAs user of the content
sudo chmod -R 770 /data/digitalvalue                               # not readable by other users
```

* **Owner.** The owner must be the user the content *runs as*. That is `rstudio-connect` by default; check *Content → Settings → Access → Who runs this content* (`Applications.RunAs` in the Connect configuration).
* **Directory permissions.** DuckDB writes a `.wal` file next to the database, so the **directory** must be writable, not only the file.
* **Location.** Avoid `/home` and the Connect data directory (`/var/lib/rstudio-connect`). Connect sandboxing can hide them from content processes.
* **Access.** The file contains every initiative and the activity log. Keep the folder restricted to the RunAs user, and back it up like any business database.

### 2.2 Point the app to it

Set these content variables (*Content → Settings → Vars*), see [section 7](#7-environment-variables):

```
DV_DB_PATH    = /data/digitalvalue/digitalvalue.duckdb
DV_BACKUP_DIR = /data/digitalvalue/backups
DV_SEED_DEMO  = false        # important: otherwise an empty database is filled with demo data
```

When the app starts it creates the file if needed, creates or migrates the schema (`db_init()`), applies the pending data migrations, and takes the daily backup.

**Bringing an existing database.** To start with the data from a pilot, stop the pilot app first. Then checkpoint the file so it has no pending `.wal`, and copy it:

```r
con <- DBI::dbConnect(duckdb::duckdb(), "digitalvalue.duckdb")
DBI::dbExecute(con, "CHECKPOINT"); DBI::dbDisconnect(con, shutdown = TRUE)
```

```bash
sudo cp digitalvalue.duckdb /data/digitalvalue/ && sudo chown rstudio-connect: /data/digitalvalue/digitalvalue.duckdb
```

### 2.3 Constraints of DuckDB (read before choosing the settings)

* **One process at a time.** DuckDB lets **one process** open the file read-write. So:
  * Set **Max processes = 1** ([section 6](#6-content-settings)). One R process serves many concurrent sessions.
  * The app opens the file only while a process has sessions, and releases it when the last session closes.
  * A process that finds the file locked retries for `DV_DB_LOCK_WAIT` seconds (default 30). It then shows a clear "database is locked" message.
* **One node.** Use a **single Connect node** (or pin the app to one node). File locks are not reliable over NFS/SMB across machines.
  * With **off-host execution (Kubernetes)**, the administrator mounts the shared volume into the content pods.
  * If you need high availability or several processes, move to PostgreSQL or SQL Server ([section 10](#10-further-recommendations)).

---

## 3. Configuration pins

Parameters, gates, RICE weights, the process-mining event map and the superusers are tables. On Connect they are read from **pins** and edited there without redeploying. Elsewhere the CSV files in `inst/config` are used.

| Pin | Content | Source CSV |
|---|---|---|
| `dv_parameters` | Valuation parameters and gate thresholds (`key`, `value`) | `parameters.csv` |
| `dv_rice_scales` | RICE weights (`dimension`, `level`, `ordinal`, `value`) | `rice_scales.csv` |
| `dv_event_map` | Process-mining mapping (`pattern`, `activity`, `process_phase`) | `event_map.csv` |
| `dv_roles` | Superusers, see [section 4](#4-roles-table-dv_roles) | `roles.csv` |

**Publish them once**, from your computer, as the service account:

```r
Sys.setenv(CONNECT_SERVER  = "https://connect.example.com",
           CONNECT_API_KEY = "<service account API key>")
board <- pins::board_connect()
digitalvalue::publish_config_pins(board = board)      # writes the four pins from inst/config
```

* **Access.** In Connect, open each pin and set *Access*:
  * Viewers: the account that runs the app (the service account owns them, so nothing to do if it also owns the app).
  * Collaborators: the people allowed to change parameters and roles.
* **App side.** Set `DV_PINS_BOARD=connect` on the app. If the pins are owned by another account than the app, also set `DV_PINS_OWNER=<that account>`; the app then reads `<owner>/dv_parameters`, and so on.
* **Edits.** Write a new version with `pins::pin_write()` (versions are kept, so a change can be reverted).
  * Changes to `dv_parameters`, `dv_rice_scales` and `dv_event_map` apply when the app process restarts.
  * Changes to `dv_roles` apply within 5 minutes (`DV_ROLES_TTL`), with no restart.

The app reads the pins with `CONNECT_SERVER` and `CONNECT_API_KEY`, which Connect normally injects into content. If your administrator has disabled this, add `CONNECT_API_KEY` as a content variable, with a key of the service account.

---

## 4. Roles table (`dv_roles`)

### 4.1 Which roles exist

| Role | How it is assigned | Can |
|---|---|---|
| **Contributor** (everyone) | Any user with access to the app | Register an initiative with its RICE |
| **Owner** | *Relational*: the initiative's `owner` (chosen at registration), or its creator | Record the 4MC valuation, send it to a validator, mark the initiative delivered |
| **Validator** | *Relational*: the user a pending 4MC validation was sent to | Validate the 4MC figures or request changes |
| **Superuser** | **Roles table** `dv_roles` | Everything (edit locked records, expert review, value audit, revert a delivery, model calibration, Admin tabs) except validating a 4MC sent to someone else; they can re-assign it |

* **Relational roles.** Owner and validator depend on the initiative, so they are stored with the initiative data and **are not in the roles table**.
* **Superusers.** Only superusers are configured. The full matrix is `permission_matrix()` in `R/fct_roles.R`, shown in *Admin → Parameters*.

### 4.2 Table definition

| Column | Type | Allowed values | Meaning |
|---|---|---|---|
| `type` | text | `user`, `group` | Whether `principal` is a Connect user name or a Connect group name |
| `principal` | text | e.g. `cplan`, `dv-superusers` | Exactly as Connect reports it to the app (`session$user`, `session$groups`); case-sensitive |
| `role` | text | `superuser` | The only assignable role (everyone else is a contributor) |
| `comment` | text | free | Optional, e.g. who approved the assignment and when |

* **Validation.** Values are trimmed, and `type` / `role` are lower-cased. Empty `principal` rows are ignored. Any other `type` or `role` stops the app at start-up with an explicit error (`normalise_roles()`).

Example (`inst/config/roles.csv` has the same columns and no rows):

```csv
type,principal,role,comment
group,dv-superusers,superuser,Planning and digital office (ticket IT-1234)
user,cplan,superuser,Corporate planning lead
```

**Recommendation.** Assign a **group** rather than individual users, and manage who is in the group in Connect (or in your identity provider). The pin then rarely changes.

### 4.3 Publish and update

```r
board <- pins::board_connect()
roles <- data.frame(
  type      = c("group", "user"),
  principal = c("dv-superusers", "cplan"),
  role      = "superuser",
  comment   = c("Planning and digital office", "Corporate planning lead"))
pins::pin_write(board, roles, "dv_roles", type = "csv",
                description = "Digital Value superusers (type user/group, principal, role, comment)")

pins::pin_versions(board, "dv_roles")       # history: who changed the roles and when
```

### 4.4 Moving from environment variables

`DV_SUPERUSERS` (users, comma-separated) and `DV_SUPERUSER_GROUP` (groups, comma-separated) are still read and **merged** with the table. To move:

1. Publish `dv_roles` with the same users and groups.
2. Open *Admin → Parameters*. It lists the effective superusers and your own user and groups, as the app sees them.
3. Remove the two variables from the content settings.

### 4.5 Safety default

* **On Posit Connect.** If no superuser is configured at all (empty pin, unreadable pin, no variables), **nobody is superuser**. The app does not fall back to "everyone is superuser", which is the local development behaviour.
* **Unreadable pin.** A warning is written in the content log. While the app is running it keeps the last roles it read successfully; at start-up an unreadable pin means no superusers.

### 4.6 Groups

* **When group names reach the app.** Group-based roles work only if Connect knows the user's groups: Connect groups, or groups from LDAP/SAML/OIDC when your administrator has enabled them.
* **How to check.** *Admin → Parameters* shows "your Connect groups". If it says *none* for a member of the group, ask your administrator, or list the users instead.

---

## 5. Deploy the app

The app is an R package with an `app.R` entry point. The bundle contains only what is needed:

```r
rsconnect::addServer("https://connect.example.com", name = "connect")
rsconnect::connectApiUser(server = "connect", account = "svc-digitalvalue",
                          apiKey = "<service account API key>")

rsconnect::deployApp(
  appDir   = ".",
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R", "inst"),
  appName  = "digital-value",
  appTitle = "Digital Value",
  server   = "connect", account = "svc-digitalvalue")
```

* **Packages.** Connect installs the packages recorded in the manifest: the `Imports` of `DESCRIPTION`, plus `pkgload` and `pins`, which are referenced in `app.R`.
* **What happens next.** The first deployment starts the app with the defaults: a local database inside the bundle, CSV configuration, demo data. Set the variables of [section 7](#7-environment-variables) right away. Saving them restarts the app with the shared database and the pins.
* **Later deployments.** Run the same `deployApp()` call; rsconnect remembers the target. Read [section 9.1](#91-redeploying) first.
* **Variables at deploy time.** Alternatively, `deployApp(envVars = c("DV_DB_PATH", ...))` copies these variables from your local environment, so the deployment is fully scripted.

---

## 6. Content settings

| Setting | Value | Why |
|---|---|---|
| **Access → Sharing** | *All users – login required*, or *Specific users or groups* | Every action is attributed to the logged-in user. **Never** allow anonymous access. |
| **Access → Who runs this content** | Default RunAs user (or the one that owns `/data/digitalvalue`) | Must be able to write the database folder. |
| **Runtime → Max processes** | **1** | DuckDB single-writer ([2.3](#23-constraints-of-duckdb-read-before-choosing-the-settings)). |
| **Runtime → Min processes** | 0 or 1 | 1 keeps R warm, so the first user does not wait for start-up. An idle process does not hold the database. |
| **Runtime → Max connections per process** | 50 | Sessions are light: one shared connection and a 5 s change check per session. Raise it if many people use the app at once. |
| **Runtime → Initial timeout** | 120 s | Covers the database lock wait (30 s) plus migrations and the first model calibration. |
| **Runtime → Idle timeout per process** | default (5 min) | Frees the process, and therefore the database, when nobody uses the app. |
| **Info → Content URL** | e.g. `/digital-value/` | A stable, readable link. |
| **Tags** | e.g. *Digital office* | Discoverability in the Connect catalogue. |

---

## 7. Environment variables

Set them in *Content → Settings → Vars*. Saving restarts the app processes.

| Variable | Production value | Default | Purpose |
|---|---|---|---|
| `DV_DB_PATH` | `/data/digitalvalue/digitalvalue.duckdb` | `digitalvalue.duckdb` (inside the bundle) | Database file |
| `DV_SEED_DEMO` | `false` | `true` | Fill an empty database with demo data |
| `DV_BACKUP_DIR` | `/data/digitalvalue/backups` | *(none: no backup)* | Daily export folder |
| `DV_BACKUP_KEEP` | `30` | `14` | Number of daily backups kept |
| `DV_DB_LOCK_WAIT` | `30` | `30` | Seconds to wait for a database locked by another process |
| `DV_PINS_BOARD` | `connect` | *(none: CSV files)* | Read the configuration from pins (`connect`, or a folder path for `board_folder()`) |
| `DV_PINS_OWNER` | owner of the pins, if different from the app owner | *(none)* | Pins are read as `<owner>/<name>` |
| `DV_ROLES_TTL` | `300` | `300` | Seconds between reloads of `dv_roles` |
| `CONNECT_API_KEY` | only if Connect does not inject it | injected by Connect | Pins and user directory (`GET /__api__/v1/users`) |
| `DV_SUPERUSERS`, `DV_SUPERUSER_GROUP` | *(remove after moving to `dv_roles`)* | *(none)* | Bootstrap superusers, merged with `dv_roles` |
| `DV_DB_DRIVER` | *(leave empty)* | `duckdb` | `sqlite` for tests only |
| `DV_USERS_FILE` | *(leave empty)* | `inst/config/users.csv` | User list used outside Connect |
| `DV_DEV_ROLE` | *(leave empty)* | *(none)* | Local development only |

`CONNECT_SERVER` and `RSTUDIO_PRODUCT` are set by Connect itself. The app uses `RSTUDIO_PRODUCT=CONNECT` to apply the safety default of [4.5](#45-safety-default).

---

## 8. Go-live checks

Open the app as a superuser and as a normal user:

1. **Header.** It shows your Connect user name and your level (SUPERUSER / CONTRIBUTOR).
2. **Admin → Parameters.**
   * Configuration source: **PINS**.
   * The effective superusers are listed, along with your user and groups.
3. **Initiative → New initiative.** The *Owner* search lists the Connect users. If it lists the demo names (`aruiz`, `dval`…), the user directory is not reachable: check `CONNECT_API_KEY`.
4. **Database.**
   * Register a test initiative, then check that `/data/digitalvalue/digitalvalue.duckdb` was updated.
   * Check that `backups/<today>` exists.
5. **Second browser.** Open the app as a second user; the test initiative appears within 5 s.
6. **4MC validation.** Send a 4MC valuation to a colleague; it appears in their *4MC validations waiting for you* box.
7. **Logs.** Look at the content logs (*Logs* tab): no errors, no `Pin dv_roles not readable` warning.
8. **Clean up.** Delete the test initiative, or leave it: as a superuser, change its registration to make it obvious.

---

## 9. Operations

### 9.1 Redeploying

* **What happens.** When a new version is deployed, Connect starts a new process for new visitors. The previous process can stay alive while its sessions are open, and it keeps the database file.
* **The new process waits.** It waits `DV_DB_LOCK_WAIT` seconds for the file; a user who arrives in that window may see the "database is locked" message.
* **Recommended practice:**
  1. Deploy when the app is quiet, and tell users to reload.
  2. If someone sees the lock message, they reload once the old sessions are closed. A Connect administrator can also stop the old process from the content's *Logs* / processes view.
* **Schema and data migrations** run automatically at start-up, and only once (table `schema_migrations`). Take a backup before a release that changes the data model.

### 9.2 Backups

* **What the app does.** With `DV_BACKUP_DIR` set, the app writes `backups/YYYY-MM-DD/` once a day, when the first session of the day starts. Each backup is a DuckDB `EXPORT DATABASE`: schema plus one Parquet file per table. The app keeps the last `DV_BACKUP_KEEP` backups.
* **Failures.** A failed backup is logged as a warning and never stops the app.
* **Off-server copy.** Also include `/data/digitalvalue/backups` in the server's backup or snapshot policy. A backup on the same disk is not enough.
* **Do not copy the live file.** Copy the exported backups, not the `.duckdb` file while the app is running.

### 9.3 Restore

```r
# 1. build a new database from a backup folder
con <- DBI::dbConnect(duckdb::duckdb(), "/data/digitalvalue/digitalvalue-restored.duckdb")
DBI::dbExecute(con, "IMPORT DATABASE '/data/digitalvalue/backups/2026-10-12'")
DBI::dbDisconnect(con, shutdown = TRUE)
```

2. Set `DV_DB_PATH=/data/digitalvalue/digitalvalue-restored.duckdb` in the content variables. Saving restarts the app on the restored file.
3. Keep the old file until the restore has been checked.

### 9.4 Monitoring and housekeeping

* **Content logs** (*Logs* tab): start-up, migrations, backup and pin warnings.
* **Usage**: the Connect usage dashboard. The app's own process log is in *Admin → Process log*.
* **`event_log` growth.** The table grows with every interaction. Export it periodically (*Admin → Process log*) and purge old raw events (for example, older than 12 months) if size becomes an issue.

---

## 10. Further recommendations

1. **Staging copy.** Deploy a second content item (e.g. `digital-value-test`) with its own `DV_DB_PATH` and `DV_SEED_DEMO=true`. Use it to try new versions and to train users. Never point two content items at the same DuckDB file.
2. **Reproducible packages.** Deploy from a tagged commit, with a fixed Package Manager snapshot (or an `renv` lockfile), so that redeploying the same version installs the same packages.
3. **Git-backed deployment.** Optional: commit a `manifest.json` (`rsconnect::writeManifest(appFiles = ...)`) and let Connect deploy from the repository branch. Every deployment then matches a commit.
4. **Server database when usage grows.** Move to PostgreSQL or SQL Server if you need:
   * more than one process;
   * several Connect nodes or high availability;
   * reporting tools reading the data live.

   All SQL is in `R/data_db.R`: add a driver branch in `db_connect()` (e.g. `odbc::odbc()` with a DSN). The SQL is ANSI with `?` placeholders, and timestamps are ISO text. The daily backup then becomes the database server's job.
5. **Least privilege.**
   * Restrict app access to the people concerned (a Connect group).
   * Make only owners of the parameters collaborators on the pins.
   * Keep the database folder `770`.
6. **User directory permissions.** The user search needs an API key allowed to list users. If your Connect restricts user listing for viewers, use the service account's key in `CONNECT_API_KEY`.
7. **Data protection.** The activity log records user names and actions, for process mining. Tell users, and agree a retention period with your data-protection officer.
