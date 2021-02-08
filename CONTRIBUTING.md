Contributing to Roots Projects

Please take a moment to review this document in order to make the contribution process easy and effective for everyone involved.

Following these guidelines helps to communicate that you respect the time of the developers managing and developing this open source project. In return, they should reciprocate that respect in addressing your issue or assessing patches and features.
Using the issue tracker

The issue tracker is the preferred channel for bug reports, features requests and submitting pull requests, but please respect the following restrictions:

    Please do not use the issue tracker for personal support requests (use the Roots Discourse to ask the Roots Community for help, or if you want the Roots Team to dedicate some time to your issue, we offer our services as well).

    Please do not derail or troll issues. Keep the discussion on topic and respect the opinions of others.

Bug reports

A bug is a demonstrable problem that is caused by the code in the repository. Good bug reports are extremely helpful - thank you!

Guidelines for bug reports:

    Use the GitHub issue search — check if the issue has already been reported.

    Check if the issue has been fixed — try to reproduce it using the latest master or development branch in the repository.

    Isolate the problem — make sure that the code in the repository is definitely responsible for the issue.

A good bug report shouldn't leave others needing to chase you up for more information. Please try to be as detailed as possible in your report.

Feature requests

Feature requests are welcome. But take a moment to find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince the Roots developers of the merits of this feature. Please provide as much detail and context as possible.

Pull requests

Good pull requests - patches, improvements, new features - are a fantastic help. They should remain focused in scope and avoid containing unrelated commits.

Please ask first before embarking on any significant pull request (e.g. implementing features, refactoring code), otherwise you risk spending a lot of time working on something that the developers might not want to merge into the project.

Please adhere to the coding conventions used throughout the project (indentation, comments, etc.).

Adhering to the following this process is the best way to get your work merged:

    Fork the repo, clone your fork, and configure the remotes:

    # Clone your fork of the repo into the current directory
    git clone https://github.com/<your-username>/<repo-name>
    # Navigate to the newly cloned directory
    cd <repo-name>
    # Assign the original repo to a remote called "upstream"
    git remote add upstream https://github.com/<upsteam-owner>/<repo-name>

    If you cloned a while ago, get the latest changes from upstream:

    git checkout <dev-branch>
    git pull upstream <dev-branch>

    Create a new topic branch (off the main project development branch) to contain your feature, change, or fix:

    git checkout -b <topic-branch-name>

    Commit your changes in logical chunks. Please adhere to these git commit message guidelines or your code is unlikely be merged into the main project. Use Git's interactive rebase feature to tidy up your commits before making them public.

    Locally merge (or rebase) the upstream development branch into your topic branch:

    git pull [--rebase] upstream <dev-branch>

    Push your topic branch up to your fork:

    git push origin <topic-branch-name>

    Open a Pull Request with a clear title and description.
