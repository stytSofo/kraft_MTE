# SPDX-License-Identifier: BSD-3-Clause
#
# Authors: Alexander Jung <a.jung@lancs.ac.uk>
#
# Copyright (c) 2021, Lancaster University.
#                     All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
from __future__ import absolute_import
from __future__ import unicode_literals

from .compartment import Compartment
from .compartment import CompartmentManager
from .hardening import Hardening
from .mechanism import Mechanism

from kraft.logger import logger

import pkg_resources
import tempfile
import subprocess
import fileinput
import filecmp
import shutil
import os

def get_sec_replacement(path=None):
    if path is None:
        raise ValueError("expected path")\
    
    return pkg_resources.resource_string(__name__,
        "replacements/%s" % path).decode("utf-8")

def get_sec_rule(path=None):
    if path is None:
        raise ValueError("expected path")
    return pkg_resources.resource_string(__name__,
        "rules/%s" % path).decode("utf-8")

def textual_replacement(compartments, template_path, filep, marker, fulldiff=None):
    template = get_sec_replacement(template_path)
    replacement = ""
    for comp in compartments:
        replacement += template.replace("{{ comp_nr }}", str(comp.number))

    backup_src = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".bak"
    shutil.copyfile(filep, backup_src)

    with fileinput.FileInput(filep, inplace=True) as file:
        for line in file:
            print(line.replace(marker, replacement), end='')

    if (not filecmp.cmp(backup_src, filep) and fulldiff is not None):
        cmd = ["diff", "-urNp", backup_src, filep]
        subprocess.call(cmd, stdout=fulldiff, stderr=subprocess.STDOUT)
        logger.info("Rewritten " + filep + " (simple replacements), backup: " + backup_src)
    else:
        # no changes: no need for a backup
        os.remove(backup_src)

def add_local_linkerscript(lib, fulldiff=None, discard_local_text=False):
    linker_script = get_sec_replacement("localextra.ld.in").replace(
            "{{ comp_nr }}", str(lib.compartment.number))

    if discard_local_text:
        linker_script = linker_script.replace(
            "/* discard rules would come here */",
            "SECTIONS\n"
            "{\n\t/DISCARD/ : {\n\t\t*(.text_comp%s .text_comp%s.*)\n\t}\n}" % (
                str(lib.compartment.number), str(lib.compartment.number)
            )
        )

    if not os.path.exists(lib.localdir):
        logger.info("Not considering %s / %s for local script rewriting: %s" % (
            lib.name, lib.libname, lib.localdir))
        return

    with open(os.path.join(lib.localdir, "flexos_extra.ld"), "w") as f:
        f.write(linker_script)

    if fulldiff is not None:
        cmd = ["diff", "-urNp", "/dev/null", os.path.join(lib.localdir, "flexos_extra.ld")]
        subprocess.call(cmd, stdout=fulldiff, stderr=subprocess.STDOUT)
        logger.info("Wrote per-lib extra linker script " +
            os.path.join(lib.localdir, "flexos_extra.ld"))

    backup_src = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".bak"
    shutil.copyfile(os.path.join(lib.localdir, "Makefile.uk"), backup_src)

    found = False
    n = lib.kname.replace("-", "")
    with open(os.path.join(lib.localdir, "Makefile.uk"), "r") as file:
        for line in file:
            if n + "_LDFLAGS-y += -Wl,-T,$(" + n + "_BASE)/flexos_extra.ld" in line:
                found = True

    if not found:
        with open(os.path.join(lib.localdir, "Makefile.uk"), "a") as f:
            f.write("\n" + n + "_LDFLAGS-y += -Wl,-T,$(" + n + "_BASE)/flexos_extra.ld")
        if fulldiff is not None:
            logger.info("Updated Makefile accordingly at %s" %
                    os.path.join(lib.localdir, "Makefile.uk"))

    if fulldiff is not None:
        cmd = ["diff", "-urNp", backup_src, os.path.join(lib.localdir, "Makefile.uk")]
        subprocess.call(cmd, stdout=fulldiff, stderr=subprocess.STDOUT)

def coccinelle_rewrite(lib, rule_file, fulldiff):
    for file in lib.files:
        # should we transform this file?
        # note: not super clean, but it does what we want: reduce the number
        # files that we look into greater detail. False positives are OK.
        try:
            with open(file, 'rb') as f:
                f_content = f.read()
                if ('<flexos/isolation.h>' not in str(f_content) and
                    '<flexos/impl/intelpku.h>' not in str(f_content)):
                    continue
        except(FileNotFoundError):
            logger.info("Possible error: file " + file + " does not exist.")
            continue

        backup_src = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".bak"
        shutil.copyfile(file, backup_src)
        log = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".log"

        with open(log, 'wb') as logf:
            try:
                cmd1 = ["spatch", "-j", "6", "-in_place", "-sp_file", rule_file, file]
                cmd2 = ["diff", "-urNp", backup_src, file]
                logf.write(bytes("$ " + " ".join(cmd1) + "\n", 'utf-8'))
                logf.flush()
                subprocess.check_call(cmd1, stdout=logf, stderr=subprocess.STDOUT)
                subprocess.call(cmd2, stdout=fulldiff, stderr=subprocess.STDOUT)
            except(subprocess.CalledProcessError):
                logger.error("Error executing subprocess, log: " + log)

        # FIXME FLEXOS we need to do this to workaround a bug in Coccinelle...
        with fileinput.FileInput(file, inplace = True) as f:
            for line in f:
                print(line.replace("__attribute__((section(\".text_shared\")))",
                                " __attribute__((section(\".text_shared\"))) "), end ='')

        if (not filecmp.cmp(backup_src, file)):
            logger.info("Rewritten " + file + ", backup: " + backup_src +
                        ", log: " + log)
        else:
            # no changes: no need for a backup
            os.remove(backup_src)
