Overview
========

Software update is a protocol mechanism that allows nodes to agree on software changes without altering protocol constants and update to this software version.
1. Software update is proposed. Proposal is a datatype that gets into
   the blockchain. It contains information about version changes and
   hashes of update files.
2. Update files are uploaded to the S3 bucket.
3. Software update is confirmed by voting from majority of nodes.
4. Nodes that see an update try to download and apply it
   automatically.

Now, again, step-by-step.

Prerequisites
=============

0. For better understanding of update system, read cardanodocs:
  * https://cardanodocs.com/cardano/update-mechanism/
  * https://cardanodocs.com/technical/updater/
1. Cluster nodes' keys. They are crucial to have since cluster nodes
   are the only nodes that have stake in the system so they are the
   only nodes that can propose an update and vote for it. It's enough
   to have keys for majority of stake. In mainnet we have 7 core nodes
   with equal stake, so 4 keys should be enough.
2. Access to the S3 bucket
3. Installers corresponding to software version we're pushing to cluster
   - Those are two executables (exe for win, pkg for mac) which are to
     be provided by QA (after QA procedures passed). They are
     installers provided by our CI.
   - Advice: after providing installers, ask QA to confirm hashes are
     same, we don't want to ship users incorrect installers to users.
4. Software version
   - Single integer, must be greater by 1 than the last confirmed
     version. Initially `csl-daedalus` application has version 0. The
     first update should have version 1, the next one 2, etc. If you
     try to propose an update with version 3 while the last confirmed
     one is 1, your update proposal won't be accepted by the nodes.
   - Node knows its software version from `configuration.yaml` file,
     `applicationVersion` field. When you propose an update to version
     X, make sure that all attached installers use configuration with
     `applicationVersion` set to `X`.

Proposing the update
====================

Ensure your keys are somewhere around and named, say, `nodeX.key`,
where `X ∈ [1..7]`. Procedure is done using `cardano-auxx`
executable, branch's code should be compatible with cluster (take release branch, e.g. `cardano-sl-1.0`).

You should change param `--peer` for all commands listed below (it is a host of any relay node). Also consider changing `--log-config`, `--logs-prefix`.
Note `--system-start 0` is perfectly valid because we don't need actual value for `cardano-auxx`.

Import secret keys: only 4 of 7 is needed. Cluster nodes all have equal stake and more than a half stake's votes is needed to make a decision about update (approve/dismiss). Imported keys are stored locally (in `secret.key` which you can delete later).

```
stack exec cardano-auxx -- --system-start 0 --log-config
  scripts/log-templates/log-config-qa.yaml
  --logs-prefix "logs/auxx-update-1.0.1" --db-path auxx-update-1.0.1
  --peer <relay dns name>:3000 cmd --commands "add-key node1.key, add-key node2.key, add-key node3.key, add-key node4.key, listaddr"
```

Propose an update:

```
stack exec cardano-auxx -- --system-start 0 --log-config
  scripts/log-templates/log-config-qa.yaml
  --logs-prefix "logs/auxx-update-1.0.1" --db-path auxx-update-1.0.1
  --peer <relay dns name>:3000 cmd --commands "propose-update 0 0.0.0 0 20 2000000 csl-daedalus:1 win64 daedalus1.exe none macos64 daedalus1c.pkg none"
```

Syntax and semantics of `propose-update` command:

```
propose-update <N> <block ver> <script ver> <slot duration> <max block size> <software ver> <propose_file>?
```

* First argument is index of imported key you're sending update
from. Step "import secret keys" has `listaddr` command in the end. Check it's output -- any cluster node will
do (`0` states for first key imported).
* Parameters 2-5 are block version data parameters -- leave them as
they are provided in the cli example.
* `csl-daedalus:1` is software version description, you should substitute `1` (version) with the integer provided along with installers (see *Prerequisites* section, *4.* item)
* End of command: list of triples -- update installers. Provide the path to installers
without any slashes (installers should be in the same dir as auxx is
launched in). First tuple element is platform, second is path, `none` stands for binary diff (this feature is not used for now).

That's it. Successfull command output looks like this:

```
[smart-wallet:DEBUG] [2017-09-21 15:13:24 MSK] Proposing update...
Read file installer062win64.exe succesfuly, its hash: 01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e
Read file installer062macos64.pkg succesfuly, its hash: 3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73
[smart-wallet:INFO] [2017-09-21 15:13:25 MSK] Announcing proposal with id 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
Update proposal submitted, upId: 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Note:

```
Read file installer062win64.exe succesfuly, its hash: 01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e
Read file installer062macos64.pkg succesfuly, its hash: 3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73
```

These hashes should be used to form URLs for installers on S3
buckets. In this example `<update
server>/01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e`
should respond with the contents of `installer062win64.exe` and
`<update
server>/3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73`
should respond with the contents of `installer062macos64.pkg`.

These hashes are Blake2b_256 hashes of CBOR-encoded contents of the files.
There is a simple command to calculate the hash of an installer:

```
stack exec cardano-auxx -- --system-start 0 --log-config
  scripts/log-templates/log-config-qa.yaml
  --logs-prefix "logs/auxx-update-1.0.1" --db-path auxx-update-1.0.1
  cmd --commands "hash-installer <FILEPATH>"
```

Note:
```
Update proposal submitted, upId: 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Value `upId` is used on one of next steps, when we'll vote for update.

Uploading update files
======================

Upload installers to S3 bucket:
* For staging:
    * URL: https://s3.eu-central-1.amazonaws.com/update-system-testing/
    * S3 Bucket: `update-system-testing`
* For mainnet:
    * URL: https://update.cardano-mainnet.iohk.io
    * S3 Bucket: `update.cardano-mainnet.iohk.io` in mainnet AWS role

Voting for proposal
===================

```
stack exec cardano-auxx -- --system-start 0 --log-config
  scripts/log-templates/log-config-qa.yaml
  --logs-prefix "logs/auxx-update-1.0.1" --db-path auxx-update-1.0.1
  --peer <relay dns name>:3000 cmd --commands "vote 1 y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6,vote 2 y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6,vote 3 y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6,vote 4 y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6"
```

In `vote N y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6`:

* `N` should be index of key (from `listadr`)
* `y` is for yes
* `4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6` is update proposal id or `upId`

Successfull output ends in "submitted a vote". Votes will be sent to
the network, software update will apply soon (after `k` blocks, for
more details read cardanodocs, links are in prerequisites).
