# SPDX-License-Identifier: BSD-3-Clause
#
# Authors: Alexander Jung <alexander.jung@neclab.eu>
#
# Copyright (c) 2020, NEC Europe Laboratories GmbH., NEC Corporation.
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

import os
import sys
import subprocess
import multiprocessing

import click

from kraft.app import Application
from kraft.cmd.list import kraft_list_preflight
from kraft.const import UNIKRAFT_BUILDDIR
from kraft.logger import logger
from kraft.util import make_progressbar
from kraft.sec.driver.vmept import VMEPTDriver
from kraft.sec import textual_replacement
from kraft.sec import coccinelle_rewrite
from kraft.sec import add_local_linkerscript

def _kraft_build_ept(app, no_ept, verbose, fast=False):
    def rewrite_linker_script(compartment):
        # 1. reset the linker script (so that we can rewrite it again)
        subprocess.call("cd %s && git checkout plat/kvm/x86/link64.lds.S" %
                app._config.unikraft.localdir, shell=True,
                stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

        # 2. apply textual replacements
        filep = os.path.join(app._config.unikraft.localdir,
                "plat/kvm/x86/link64.lds.S")

        def simple_replace(template_path, marker):
            comps = list(set(app.compartments) - set([c for c in app.compartments if c.default]))
            textual_replacement(comps, template_path, filep, marker)

        simple_replace(
            "linkerscript_data.in",
            "/* __FLEXOS MARKER__: insert compartment data sections here. */")
        simple_replace(
            "linkerscript_bss.in",
            "/* __FLEXOS MARKER__: insert compartment bss sections here. */")
        simple_replace(
            "linkerscript_initarray_ept.in",
            "/* __FLEXOS MARKER__: insert compartment init array sections here. */")

        # backup global linker script for debugging
        subprocess.call(("cd %s && cp plat/kvm/x86/link64.lds.S plat/kvm/x86/link64.lds.S.comp" + str(i)) %
                app._config.unikraft.localdir, shell=True,
                stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

    appcomp = None
    for lib in app.libraries:
        if lib.name.startswith("app"):
            appcomp = lib.compartment

    i = 0
    logger.info("Building with VM/EPT: " + str(no_ept) + " images to build.")
    for comp in app.compartments:
        logger.info("Building image " + str(i) + "/" + str(no_ept - 1))

        # edit build args
        extra_args = ["CFLAGS_EXTRA=-DFLEXOS_VMEPT_COMP_ID=" + str(i) 
                    + " -DFLEXOS_VMEPT_COMP_COUNT=" + str(no_ept)
                    + " -DFLEXOS_VMEPT_APPCOMP=" + str(appcomp.number)]
        if fast:
            extra_args.insert(0, "-j")

        # rewrite linker script
        rewrite_linker_script(comp)

        subprocess.call("make clean", shell=True)

        # build
        return_code = make_progressbar(app.make_raw(verbose=verbose,
            extra=extra_args))

        if (return_code > 0):
            # there was probably an error
            logger.error("Aborting build due to probable error during execution "
                         "of the make command (code " + str(return_code) + ")")
            return

        # secure image (so that it doesn't get overwritten)
        for target in app.binaries:
            os.rename(target.binary, target.binary + ".comp" + str(i))
            os.rename(target.binary_debug, target.binary_debug + ".comp" + str(i))

        print("\nSuccessfully built image " + str(i) + "/" + str(no_ept - 1))
        i += 1

    # output list of images
    print("\nSuccessfully built all VM/EPT images:\n")
    for target in app.binaries:
        i = 0
        for comp in app.compartments:
            print("  => %s/%s" % (
                UNIKRAFT_BUILDDIR,
                os.path.basename(target.binary) + ".comp" + str(i)
            ))
            i += 1

        i = 0
        for comp in app.compartments:
            print("  => %s/%s (with symbols)" % (
                UNIKRAFT_BUILDDIR,
                os.path.basename(target.binary_debug) + ".comp" + str(i)
            ))
            i += 1

    # done building EPT images
    return

@click.pass_context
def kraft_build(ctx, verbose=False, workdir=None, fetch=True, prepare=True,
                progress=True, target=None, fast=False, force_build=False,
                compartmentalize=False):
    """
    """
    if workdir is None or os.path.exists(workdir) is False:
        raise ValueError("working directory is empty: %s" % workdir)

    logger.debug("Building %s..." % workdir)

    app = Application.from_workdir(workdir, force_build)

    if not app.is_configured():
        if click.confirm('It appears you have not configured your application.  Would you like to do this now?', default=True):  # noqa: E501
            app.configure()

    n_proc = None
    extra = []
    if fast:
        # This simply set the `-j` flag which signals to make to use all cores.
        n_proc = multiprocessing.cpu_count()
        extra = ["-j"]

    if fetch:
        app.fetch()

    for comp in app.compartments:
        with open(os.path.join(app.localdir, ".config")) as conf:
            if comp.mechanism.driver.kconfig_opt not in conf.read():
                raise ValueError("Compartment " + comp.name + " requires " + comp.mechanism.driver.kconfig_opt)

    if compartmentalize:
        app.compartmentalize()

    if prepare:
        app.prepare()

    # in the case of VM/EPT we have to build several images
    ept_images = [comp for comp in app.compartments
                      if type(comp.mechanism.driver) == VMEPTDriver]
    no_ept = len(ept_images)

    if (no_ept):
        _kraft_build_ept(app, no_ept=no_ept, verbose=verbose, fast=fast)
        return

    if progress:
        return_code = make_progressbar(app.make_raw(verbose=verbose, extra=extra))

    else:
        return_code = app.build(
            target=target,
            n_proc=n_proc
        )

    if return_code == 0:
        print("\nSuccessfully built images:\n")

        for target in app.binaries:
            if not os.path.exists(target.binary):
                continue

            print("  => %s/%s" % (
                UNIKRAFT_BUILDDIR,
                os.path.basename(target.binary)
            ))
            print("  => %s/%s (with symbols)" % (
                UNIKRAFT_BUILDDIR,
                os.path.basename(target.binary_debug)
            ))

        print("\nTo instantiate, use: kraft run\n")


@click.command('build', short_help='Build the application.')
@click.option(
    '--verbose', '-v', 'verbose_build',
    help='Verbose build',
    is_flag=True
)
@click.option(
    '--fetch/--no-fetch', 'fetch',
    help='Run fetch step before build.',
    default=True
)
@click.option(
    '--prepare/--no-prepare', 'prepare',
    help='Run prepare step before build.',
    default=True
)
@click.option(
    '--compartmentalize/--no-compartmentalize', 'compartmentalize',
    help='Run compartmentalize step before build.',
    default=False
)
@click.option(
    '--progress/--no-progress', 'progress',
    help='Show progress of build.',
    default=True
)
@click.option(
    '--fast', '-j', 'fast',
    help='Use all CPU cores to build the application.',
    is_flag=True
)
@click.option(
    '--force', '-F', 'force_build',
    help='Force the build of the unikernel.',
    is_flag=True
)
@click.argument('target', required=False, nargs=-1)
@click.pass_context
def cmd_build(ctx, verbose_build=False, fetch=True, prepare=True,
              progress=True, target=None, fast=False, force_build=False,
              compartmentalize=False):
    """
    Builds the Unikraft application for the target architecture and platform.
    """

    kraft_list_preflight()

    try:
        kraft_build(
            workdir=ctx.obj.workdir,
            verbose=verbose_build,
            fetch=fetch,
            compartmentalize=compartmentalize,
            prepare=prepare,
            progress=progress,
            target=" ".join(list(target)),
            fast=fast,
            force_build=force_build
        )

    except Exception as e:
        logger.critical(str(e))

        if ctx.obj.verbose:
            import traceback
            logger.critical(traceback.format_exc())

        sys.exit(1)
