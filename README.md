# gtd-review

### _Jonathan A. Bennett <doulos05@gmail.com>_

This is a suite of tools to guide a user through a thorough weekly review using Taskwarrior to store active tasks.

## License

MIT

## Installation

### Prerequisites

GTD-review is written in Common Lisp and requires a basic Common Lisp setup in order to run. Ensure that the following are correctly configured on your system:

1. [sbcl](http://www.sbcl.org/index.html) Steel Bank Common Lisp (GTD-Review should work with other Common Lisps, but has not been tested).
2. [Quicklisp](https://www.quicklisp.org/beta/), the package manager for Common Lisp. This is used to download and install any 3rd party Common Lisp systems needed for GTD-Review to run.

Once you have SBCL and Quicklisp setup according to the instructions on their website, follow the steps below to install GTD-review.

1. Clone the repository into your local-projects folder (typically `~/quicklisp/local-projects`).
2. `make build` to create the gtd-review executable.
3. `make install` to copy the `gtd-review` executable to `~/.bin`. If you need it to be elsewhere to be on your path, please skip this step and manually copy it.
4. (Optional) `make hook` to create and install the `taskwarrior` on-exit hook.

## Usage

### Add

Run `gtd-revew add <project>` to add `<project>` to the `/.cl-gtd/projects.txt` file. `<project>` should be in a format acceptable to Taskwarrior as a project (no spaces). This adds nothing to taskwarrior, but it does ensure that you will see this project when you do your next review.

### Projects

Run `gtd-review projects` to trigger a review of your projects. When this is called, the projects in your `projects.txt` file will be displayed one by one and the taskwarrior command `task project.is:<project> and (status:PENDING or status:WAITING) all` will be called. This gives you a list of pending and waiting tasks for this project. Review this list and evaluate the project for yourself. Do the tasks currently captured in taskwarrior represent everything you need to do with this project? Are any of these tasks stale? Using a _separate_ terminal window, add/modify/complete/delete any tasks that need to be added/modified/completed/deleted in taskwarrior. Finally, mark the project as:

- [a]ctive: This project is still part of my productivity landscape and should remain in my system.
- [c]ompleted: This project is complete and can be removed from my system.
- [d]eleted: This project is no longer relevant and can be removed from my system.

**NOTE** As of version 0.2.0, Complete and Delete have the same behavior! A future release will have different behavior for these two items.

**NOTE 2** This project does not mark tasks done or deleted in taskwarrior for you. If you mark a project completed or deleted, make the matching edits to your taskwarrior tasks immediately (before completing the review). A future release may add an option to perform this for you.

### Review

This is the meat of the program. This guides you through a weekly review checklist, ensuring at the end that you have gone through a thorough review of all the stuff in your life and categorized it appropriately. It works by presenting you with a series of prompts. In response to each prompt, you can type a task in to add it to taskwarrior. Simply enter what you would enter after the command `task add`. So to add the task "Get Milk +@errands priority:H" to your tasks, you would enter `Get Milk +@errands priority:H`. That exact string will be passed to Taskwarrior. You will see on the screen the command that is being run.

Right now, the weekly review can only be customized by editing the `review.lisp` folder. Future versions will allow you to define your own reviews (removing unnecessary prompts or creating custom reviews like a shortened "daily" review). The full review is a full, GTD style weekly review. Expect it to take a full hour if you are a reasonably broadly committed person. Pausing a review and resuming where you left off is not currently supported.

### The Hook

The hook updates your `projects.txt` file after every time that taskwarrior is run. The update is additive only, that is the hook will never remove a project from your `projects.txt` file. See the section below for why.

## Why This App

This app exists to make reviewing your tasks easier. "But what about `tasksh review`!?" I hear you ask.

### Tasksh review

`tasksh review` is a fantastic tool for looking at every single task in your queue. If you've never used it before, I suggest taking a look, it can be incredibly useful. But it can also be incredibly overwhelming. I'm a teacher and during the semester I have anywhere from 70 to 90 tasks in taskwarrior. At the mid-term and end of term grading periods, I might get up to 120-150. Even assuming I can read each task and make in intelligent decision about what modifications I need to make to that task in 10 seconds per task, that's still 10 to 25 minutes of my weekly review taken up just reviewing my tasks. And at the end of that process, I'm still going to need to go over my projects list and make sure I haven't missed any tasks - a process which will almost certainly require me to search my tasks to ensure I don't add duplicates.

`tasksh review` focuses on tasks because taskwarrior's level of abstraction is the task. This is completely appropriate and should not be changed. While doing, "task" is the ideal level of abstraction. We may say that we're "working on our final reports", but what we're actually doing is "writing the first draft" or "reviewing the data from the reporting period". And so a tool intended for use while doing must optimize it's abstractions around the task. But the task is too granular to be a useful level of abstraction for review. For that, you need to step up to the project level.

The purpose of this program is to use taskwarrior to track and complete your tasks (where the level of abstraction is optimized for doing) and use gtd-review to review and manage your projects (where the level of abstraction is optimized for reviewing).

As such, this program works best when used in conjunction with the optional on-exit hook provided above. Using it with the on-exit hook ensures that no projects "slip through" because the only tasks tied to them in taskwarrior are completed or deleted before your review. The fundamental assumption that this program makes is that projects should only be marked done intentionally. The on-exit hook ensure that this is true by syncing your projects list in taskwarrior with gtd-review.

## Roadmap

### 0.1.0

Allowed users to review their projects list.

### 0.2.0

This release.

### 0.2.1

- Add custom weekly review templates.

### 0.2.2

- Capture and store metadata about projects.

### 0.2.3

- Project View which displays your tasks for a project as well as other metadata around the project (to include links to folders where the project reference data lives).

### 0.3

- Natural planning guide and project management view.

### 0.4

- (We can already do this!) Use metadata to automatically add metadata to created tasks in taskwarrior (if a project has the @computer tag, then so do tasks created from it in gtd-review)
