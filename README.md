# Projectwarrior

**0.3.0 BREAKING CHANGE:** Because I am not sure exactly how to add a project from just the slug present in taskwarrior, the task-hook is currently depreciated. Leaving it running will not overwrite anything (It targets a different file), but any projects that it pulls from taskwarrior won't get added to projectwarrior.

### _Jonathan A. Bennett <doulos05@gmail.com>_

This is a simple command line project management tool. Users can enter the projects along with metadata about the projects and track them through to completion. It integrates with taskwarrior for tracking the tasks within those projects, but could be integrated manually with another tool if you wanted.

See our [wiki](https://github.com/jonathanabennett/projectwarrior/wiki) for more details!

## License

MIT

## Installation

### [Prerequisites](https://github.com/jonathanabennett/projectwarrior/wiki/prerequisites)

Projectwarrior is written in Common Lisp and requires a basic Common Lisp setup in order to run. Ensure that the following are correctly configured on your system:

1. [sbcl](http://www.sbcl.org/index.html) Steel Bank Common Lisp (Projectwarrior should work with other Common Lisps, but has not been tested).
2. [Quicklisp](https://www.quicklisp.org/beta/), the package manager for Common Lisp. This is used to download and install any 3rd party Common Lisp systems needed for Projectwarrior to run.

### [Installation](https://github.com/jonathanabennett/projectwarrior/wiki/install)
Once you have SBCL and Quicklisp setup according to the instructions on their website, follow the steps below to install Projectwarrior.

1. Clone the repository into your local-projects folder (typically `~/quicklisp/local-projects`).
2. `make build` to create the projectwarrior executable.
3. `make install` to copy the `projectwarrior` executable to `~/.bin`. If you need it to be elsewhere to be on your path, please skip this step and manually copy it.

## Usage

Below is a quick summary with examples for every command. More details can be found in the wiki, particularly the [philosophy](https://github.com/jonathanabennett/projectwarrior/wiki/philosophy) and [examples](https://github.com/jonathanabennett/projectwarrior/wiki/examples) pages, which get updated regularly.

### Add

`project add <details>` adds a project. Projects are stored as JSON objects in an array stored in `~/.projects/active.json`. Each object can have the following properties.

- UUID: The UUID is set programmatically by projectwarrior and can be used to identify projects uniquely. The user should never need to change this.
- ID: The project's position within the JSON array it is stored in. This updates every time the JSON file is saved and can be used to refer to projects.
- Slug: The slug is a short title suitable for use as a taskwarrior `project`. It is used by Projectwarrior when adding tasks to taskwarrior. Slugs are identified by `slug:<string>`. For example: `slug:product-launch` or `slug:springCleaning`. While slugs can be generated automatically by projectwarrior, it does so in a very naive fashion and I do not recommend it. Every project should have a slug.
- Area of Focus: An Area of Focus is a short (like a slug) tag indicating which part of your life this task belongs to. For further details, see Tiago Forte's PARA method or David Allen's Getting Things Done. Areas are identified by `area:<string>`. For example: `area:relationships`, `area:professional`, or `area:side-hustle`.
- Tags: Tags are used to classify projects. Tags are added as `+<string>`. Examples could include `+@office`, `+high-priority`, or `+vacation-home`. The primary use for tags is to filter your projects for the `project view` command. For example, if you are at work you might say `project +@office view` so you do not see any projects from your vacation home.
- Inherited Tags: Inherited tags are tags which get applied by default to all tasks created in taskwarrior by projectwarrior. They are identified by `++<string>`. For example, if you have a project that needs to appear at the top of your `task next` report, you may give that project the `++next` tag so that every task you create for that task automatically has the `next` tag applied to it. To remove a tag for an individual task (maybe this task within the project doesn't need to be urgently), simply put `-next` in the task description and the two will cancel each other out.
- Filepath: Filepath points to a file on the computer which contains notes about a project. It is added or modified by `file:path/to/file`. File can be any type which can be edited by your $EDITOR.
- Description: Anything not collected by the filters above becomes part of the Description. Beware of putting characters in your project which have special meanings on the command line. I like to make this an imperative sentence (`Launch new marketing campaign`), but you may find a descriptive sentence fits better (`The new marketing campaign is successful.`)

Examples:

`project add Reorganize garage slug:garage area:personal ++weekend`

`project add Successful spring sales conference slug:spring-sales-22 area:professional +@work`

### Mod

`project <filter> mod <details>` modifies all projects which match `<filter>`, applying the `<details>` provided. Filters and details follow the same rules as for `add` above.

One additional filter which is not relevant for add is `status`. `status:active` returns projects on the `~/.projects/active.json` list, `status:completed` returns projects on the `~/.projects/completed.json` list, and `status:deleted` returns projects on the `~/.projects/deleted.json` list.

`project <filter> modify <details>` is identical to `project <filter> mod <details>`

Example calls:

`project 14 mod +@work`

`project +@work mod area:professional`

`project status:completed area:professional mod +@review`

### Complete

`project <filter> done` moves all projects which match `<filter>` from the `~/.projects/active.json` file to the `~/.projects/completed.json` file.

If there are more than 3 projects which will be affected, you will be prompted for each one.

Example Calls

`project 14 done`

`project Sales Calls done`

### Delete

`project <filter> delete` moves all projects which match `<filter>` from the `~/.projects/active.json` file to the `~/.projects/deleted.json` file.

`project <filter> del` is identical to `project <filter> delete`

If there are more than 3 projects which will be affected, you will be prompted for each one.

Example Calls:

`project id:14 delete`

`project Sales Calls delete`

### New 
`project <filter> new <task>` adds a task via taskwarrior whose project string is the slug of the project matching `<filter>`. Filter must match exactly 1 project, I recommend selecting via the `id` field.

Example Calls:

`project id:14 new Call Acme Engineering +next due:tuesday`

### Open
`project <filter> open` will check to see if the task has a filepath associated with it. If it does, it will open that file with your shell's $EDITOR environmental variable. This is used for storing project notes in a convenient to access place. Future versions may create files according to templates.

### Reports

`project <filter> <report_name>` will show a tabular view of all projects matching `<filter>` formatted according to the report matching `<report_name>`. The ID numbers shown there can be used to complete or delete those tasks.

Running `project` will show a complete list of all your projects. Running `project <filter>` will show a list of projects matching `<filter>`.

Reports included are the default report (called `view`) and `count` which gives you a count of how many tasks are associated with each project in taskwarrior. Additional reports can be written according to the guidelines in the [configuration](https://github.com/jonathanabennett/projectwarrior/wiki/config) page of the wiki.

Examples Calls:

`project +@work view`

`project`

`project area:professional count`

### Review

This is one of the main reasons I built this program. `project review <checklist>` guides you through the `<checklist>` review checklist, ensuring at the end that you have gone through a thorough review of all the stuff in your life and categorized it appropriately. Each checklist (except `projects`, describe below) works by presenting you with a series of prompts. In response to each prompt, you can type a project in to add it to projectwarror (following the format described for the `add` command). Once you hit enter without typing anything in, you can do the same with tasks to add to taskwarrior. Simply enter what you would enter after the command `task add`. So to add the task "Get Milk +@errands priority:H" to your tasks, you would enter `Get Milk +@errands priority:H`. That exact string will be passed to Taskwarrior. You will see on the screen the command that is being run.

Right now, the weekly review can only be customized by editing the `review.lisp` file and re-running `make build && make install`. Future versions will allow you to define your own reviews (removing unnecessary prompts or creating custom reviews like a shortened "daily" review). The full review is a full, GTD style weekly review. Expect it to take a full hour if you have a lot of personal and professional commitments. Pausing a review and resuming where you left off is not currently supported, but the full review can be taken in 3 pieces like so:

1. `project review professional`
2. `project review personal`
3. `project review projects`

Doing it this way only skips the reminder to check your calendar and your someday/maybe list, and to "get creative" at the end of your review time.

The currently supported review are

- `project review weekly`: the default review
- `project review professional`: Only the professional prompts from the weekly review
- `project review personal`: Only the personal prompts from the weekly review
- `project review projects`: See below

### Projects Review

Run `project review projects` to trigger a review of your projects. This review also occurs at the end of your weekly review. When this is called, the projects in your `~/.projects/active.json` file will be displayed one by one and the taskwarrior command `task project.is:<project slug> and (status:PENDING or status:WAITING) all` will be called. This gives you a list of pending and waiting tasks for this project. Review this list and evaluate the project for yourself. Do the tasks currently captured in taskwarrior represent everything you need to do with this project? Are any of these tasks stale? If you need to add a task, you will be prompted to add a task using the project slug and the inherited tags from that project. If you need to modify or complete any existing tasks, use a _separate_ terminal window, to run the appropriate taskwarrior commands. Projectwarrior will **NEVER** remove something from your tasks list for you, this is an additive process only. Finally, mark the project as:

- [a]ctive: This project is still part of my productivity landscape and should remain `active.json`.
- [c]ompleted: This project is complete and should be moved to `completed.json`.
- [d]eleted: This project is no longer relevant and should be moved to `deleted.json`.

**NOTE** This does not mark tasks done or deleted in taskwarrior for you. If you mark a project completed or deleted, you must make the matching edits to your taskwarrior tasks.

### The Hook CURRENTLY DEPRECIATED

**NOTE:** The section below describes the hook as it existed in the 0.2 release. The code from the 0.2 release is still here. I have not yet decided how to re-write the hook to account for the new, expanded data being collected about projects in projectwarrior. Should you have suggestions, please file an issue in Github.

The hook updates your `projects.txt` file after every time that taskwarrior is run. The update is additive only, that is the hook will never remove a project from your `projects.txt` file. See the section below for why.

## Why This App

This app exists to make reviewing your tasks easier. "But what about `tasksh review`!?" I hear you ask.

### Tasksh review

`tasksh review` is a fantastic tool for looking at every single task in your queue. If you've never used it before, I suggest taking a look, it can be incredibly useful. But it can also be incredibly overwhelming. I'm a teacher and during the semester I have anywhere from 70 to 90 tasks in taskwarrior. At the mid-term and end of term grading periods, I might get up to 120-150. Even assuming I can read each task and make in intelligent decision about what modifications I need to make to that task in 10 seconds per task, that's still 10 to 25 minutes of my weekly review taken up just reviewing my tasks. And at the end of that process, I'm still going to need to go over my projects list and make sure I'm not missing any tasks - a process which will almost certainly require me to search my tasks to ensure I don't add duplicates.

`tasksh review` focuses on tasks because taskwarrior's level of abstraction is the task. This is completely appropriate and should not be changed. While _doing_, "task" is the ideal level of abstraction. We may say that we're "working on our final reports", but what we're actually _doing_ is "writing the first draft" or "reviewing the data from the reporting period". And so a tool intended for use while doing must optimize for that abstraction level. But the task is too granular to be a useful level of abstraction for _reviewing_. For that, you need to step up to the project level.

The purpose of this program is to use taskwarrior (or other todo managers) to track and complete your tasks (where the level of abstraction is optimized for doing) and use projectwarrior to review and manage your projects (where the level of abstraction is optimized for reviewing).

## Roadmap

### 0.3 Basic Projects

- Store basic information about projects.
- Incorporate this project information into the weekly review.
- Use metadata to automatically add metadata to created tasks in taskwarrior (if a project has the @computer tag, then so do tasks created from it in projectwarrior)

### 0.3.1 Expanded Projects

- Allow user configuration and create a default config.
- Change the `view` display to make it easier to read and more compact.
- Allow some customization by users of the `view` display

### 0.3.2 Supporting Documents for Projects

- Create a `project open` command which opens a file containing project support information.

### 0.4 Documentation and Demonstration 

At this point, the core features of the program will be complete enough to begin creating clear, user-facing documentation and demonstration videos/guides/documents. While I am working on written documentation as I go, this release will focus on things like:

- Creating a functional `man` page for the project.
- Creating a series of videos demonstrating various use cases.
- Writing detailed documentation which appears on Github as a wiki and is downloaded with the program as markdown files.

### 0.4.1 Expanded Reviews
- Pull reviews out of the "compiled" portion of the program so that they are user-configurable.
- Allow the users to create and register their own reviews using their config file.

### And Beyond!

- Natural planning guide
- Project management view
- Project calendar / gantt charts
- Syncing (currently you'll need to use Dropbox/Nextcloud/Syncthing/etc to sync your `.projects` folder)
- A mobile app (?)
- A web app (??)
- A TUI/GUI (?!?!)
