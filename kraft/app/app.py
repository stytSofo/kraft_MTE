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
import subprocess
import tempfile
from pathlib import Path

import click
import requests
import six
import re
import glob

import kraft.util as util
from kraft.arch import Architecture
from kraft.component import Component
from kraft.config import Config
from kraft.config import find_config
from kraft.config import load_config
from kraft.config.config import get_default_config_files
from kraft.config.serialize import serialize_config
from kraft.const import DOT_CONFIG
from kraft.const import MAKEFILE_UK
from kraft.const import SUPPORTED_FILENAMES
from kraft.const import UNIKRAFT_BUILDDIR
from kraft.const import UNIKRAFT_LIB_MAKEFILE_URL_EXT
from kraft.error import KraftError
from kraft.error import KraftFileNotFound
from kraft.error import MissingComponent
from kraft.lib import Library
from kraft.lib import LibraryManager
from kraft.logger import logger
from kraft.manifest import maniest_from_name
from kraft.plat import InternalPlatform
from kraft.plat import Platform
from kraft.plat.network import NetworkManager
from kraft.plat.volume import VolumeManager
from kraft.target import Target
from kraft.target import TargetManager
from kraft.types import break_component_naming_format
from kraft.types import ComponentType
from kraft.unikraft import Unikraft

from kraft.sec import get_sec_replacement
from kraft.sec import get_sec_rule
from kraft.sec.driver.intelpku import IntelPKUDriver
from kraft.sec.driver.vmept import VMEPTDriver
from kraft.sec.driver.fcalls import FcallsDriver
from kraft.sec import textual_replacement
from kraft.sec import coccinelle_rewrite
from kraft.sec import add_local_linkerscript

import glob
import filecmp
import tempfile
import shutil
import subprocess
import fileinput


class Application(Component):
    _type = ComponentType.APP

    _config = None
    @property
    def config(self): return self._config

    @click.pass_context  # noqa: C901
    def __init__(ctx, self, *args, **kwargs):
        super(Application, self).__init__(*args, **kwargs)

        # Determine name from localdir
        if self._name is None and self._localdir is not None:
            self._name = os.path.basename(self._localdir)

        self._config = kwargs.get('config', None)
        self.libincludes = None

        # Determine how configuration is passed to this class
        if self._config is None:
            unikraft = kwargs.get("unikraft", dict())
            if not isinstance(unikraft, Unikraft):
                unikraft = Unikraft(unikraft)

            targets = kwargs.get("targets", dict())
            if not isinstance(targets, TargetManager):
                targets = TargetManager(targets, unikraft)

            libraries = kwargs.get("libraries", dict())
            if not isinstance(unikraft, LibraryManager):
                libraries = LibraryManager(libraries)

            networks = kwargs.get("networks", dict())
            if not isinstance(networks, NetworkManager):
                networks = NetworkManager(networks)

            volumes = kwargs.get("volumes", dict())
            if not isinstance(volumes, VolumeManager):
                volumes = VolumeManager(volumes)

            self._config = Config(
                name=kwargs.get('name', None),
                arguments=kwargs.get("arguments", None),
                before=kwargs.get("before", None),
                after=kwargs.get("after", None),
                unikraft=unikraft,
                targets=targets,
                libraries=libraries,
                volumes=volumes,
                networks=networks,
            )

        # Check the integrity of the application
        if self.config.unikraft is None:
            raise MissingComponent("unikraft")

        # Initialize the location of the known binaries
        for target in self.config.targets.all():
            binname = target.binary_name(self.config.name)
            binary = os.path.join(self.localdir, UNIKRAFT_BUILDDIR, binname)
            if binname is not None:
                target.binary = binary

        if self._config is None:
            self._config = dict()

    def find_files(self):
        # This is a dirty way to get all the files
        def is_core_lib(libname, libfiles):
            def is_core_file(x):
                return x.startswith(self._config.unikraft.localdir)
            return len(list(filter(is_core_file, libfiles[libname]))) != 0

        def scan_for_files(lib, libfiles, ext):
            libpath = ""
            if is_core_lib(lib, lib_files):
                l = lib
                if l.startswith('lib'):
                    l = l[len('lib'):]
                libpath = os.path.join(os.environ['UK_ROOT'], "lib", l)
            else:
                libpath = os.path.join(os.environ['UK_LIBS'], lib)

            for file in glob.iglob(libpath + '/**/*' + ext, recursive=True):
                if file not in lib_files[lib]:
                    lib_files[lib].append(file)

            for l in self.libraries:
                if lib == l.name:
                    for file in glob.iglob(l.builddir + '/**/*' + ext, recursive=True):
                        lib_files[lib].append(file)

        if os.path.exists(self.unikraft.localdir):
            print_srcs = self.make_raw('print-srcs')
            logger.debug("Retrieving list of library files...")
            output = subprocess.check_output(" ".join(print_srcs), shell=True)

            # remove first and last lines (not part of the actual content)
            # note: -2 because of the EOF \n
            raw_files = str(output).split('\\n')[1:-2]

            raw_lib_files = {} # {lib: [file, file, ...], ...}
            for (lib,files) in zip(raw_files[0::2],raw_files[1::2]):
                # remove that weird {|common, |x86, ...} suffix from some paths...
                def rmprefix(x):
                    last = x.rfind("|")
                    if last > 0: return x[:last]
                    return x

                lst = list(map(rmprefix, list(filter(None, files.split(" ")))))
                # remove lib prefix, kraft doesn't want it
                raw_lib_files[str(lib).strip("lib").rstrip(":").strip()] = lst

            lib_files = {}
            # merge some entries: newlibc, newlibm, newlibglue should belong together
            for (lib,files) in raw_lib_files.items():
                name = lib

                for l in self.libraries:
                    l_gluedir = os.path.join(os.environ['UK_LIBS'], l.name)
                    if (len(list(filter(lambda x: x.startswith(l.builddir), files))) != 0 or
                        len(list(filter(lambda x: x.startswith(l_gluedir),  files))) != 0):
                        logger.debug("Mapping " + lib + " to " + l.name)
                        name = l.name
                        break

                if name in lib_files.keys():
                    lib_files[name].extend(files)
                else:
                    lib_files[name] = files

            # find potentially missing .c/.h files: recursively scan lib folders...
            for lib in lib_files.keys():
                scan_for_files(lib, lib_files, ".c")
                scan_for_files(lib, lib_files, ".h")

            # find the default compartment
            default_comp = list(filter(lambda x: x.default, self.compartments))[0]

            # create missing lib objects
            for (lib,files) in lib_files.items():
                # create missing lib objects
                # note: don't set hardening, if these libs had hardening they'd
                # already be instanciated
                # TODO cleanup path detection, this is a huge mess
                if lib not in [l.name for l in self.libraries]:
                    localdir = os.path.join(os.environ['UK_LIBS'], lib)
                    if is_core_lib(lib, lib_files):
                        localdir = os.path.join(os.environ['UK_ROOT'], "lib", lib[len('lib'):])
                    if (lib.startswith("app")):
                        # this is an application, another dirty workaround...
                        rootp = os.path.normpath(os.path.join(os.environ['UK_ROOT'], "..", "apps"))
                        if (len(files) > 1):
                            filep = os.path.normpath(files[0])
                            if (len(filep[len(rootp):].split("/")[0]) == 0):
                                localdir = os.path.join(rootp, filep[len(rootp):].split("/")[1])
                            else:
                                localdir = os.path.join(rootp, filep[len(rootp):].split("/")[0])

                    self.config.libraries.add(Library(
                        name=lib,
                        version=None,
                        manifest=None,
                        localdir=localdir,
                        is_core=is_core_lib(lib, lib_files),
                        compartment=default_comp
                    ))

            # finally, add the library
            for l in self.libraries:
                lname = l.name
                if lname not in lib_files: lname = "lib%s" % lname
                for f in lib_files[lname]: l.add_file(f)

    def find_includedirs(self):
        # TODO same todos as find_files()...
        if os.path.exists(self.unikraft.localdir):
            makep = self.make_raw('-pn')
            logger.debug("Retrieving list of library include dirs...")
            output = subprocess.check_output(" ".join(makep), shell=True)

            self.libincludes = list(set(map(lambda x: x.strip().rstrip("\\\\n#"),
                re.findall("\-I(/root/[^\s]*)", str(output)))))

    @classmethod  # noqa: C901
    @click.pass_context
    def from_workdir(ctx, cls, workdir=None, force_init=False, use_versions=[]):
        if workdir is None:
            workdir = ctx.obj.workdir

        config = load_config(
            find_config(workdir, None, ctx.obj.env),
            use_versions=use_versions
        )

        return cls(
            config=config,
            localdir=workdir,
            ignore_version=force_init,
        )

    @property
    @click.pass_context
    def builddir(ctx, self):
        if self.localdir is None:
            raise None

        if not os.path.exists(ctx.obj.workdir):
            return None

        builddir = os.path.join(ctx.obj.workdir, UNIKRAFT_BUILDDIR)
        if not os.path.exists(builddir):
            return None

        return builddir

    @property
    def components(self):
        components = list()

        components.append(self.config.unikraft)

        for target in self.config.targets.all():
            components.append(target.architecture)
            components.append(target.platform)

        for library in self.config.libraries.all():
            components.append(library)

        return components

    @property
    def unikraft(self):
        return self.config.unikraft

    @property
    def targets(self):
        return self.config.targets.all()

    @property
    def compartments(self):
        return self.config.compartments.all()

    @property
    def libraries(self):
        return self.config.libraries.all()

    @property
    def manifests(self):
        manifests = list()
        components = self.components

        for component in components:
            if component.manifest is not None:
                manifests.append(component.manifest)

        return manifests

    def is_configured(self):
        if os.path.exists(os.path.join(self._localdir, DOT_CONFIG)) is False:
            return False

        return True

    def open_menuconfig(self):
        """
        Run the make menuconfig target.
        """
        cmd = self.make_raw('menuconfig')
        logger.debug("Running:\n%s" % ' '.join(cmd))
        subprocess.run(cmd)

    def make_raw(self, extra=None, verbose=False):
        """
        Return a string with a correctly formatted make entrypoint for this
        application.
        """

        cmd = [
            'make',
            '-C', self.config.unikraft.localdir,
            ('A=%s' % self._localdir)
        ]

        if verbose:
            cmd.append('V=1')

        plat_paths = []
        for target in self.config.targets.all():
            if not isinstance(target.platform, InternalPlatform):
                plat_paths.append(target.platform.localdir)

        if len(plat_paths) > 0:
            cmd.append('P=%s' % ":".join(plat_paths))

        lib_paths = []
        for lib in self.config.libraries.all():
            # terrible hack because the application is considered as a microlibrary
            # TODO Alex how can we avoid this?
            if not lib.is_core and not lib.name.startswith("app"):
                lib_paths.append(lib.localdir)

        cmd.append('L=%s' % ":".join(lib_paths))

        if type(extra) is list:
            for i in extra:
                cmd.append(i)

        elif type(extra) is str:
            cmd.append(extra)

        return cmd

    @click.pass_context
    def make(ctx, self, extra=None, verbose=False):
        """
        Run a make target for this project.
        """
        cmd = self.make_raw(
            extra=extra, verbose=verbose
        )
        return util.execute(cmd)

    @click.pass_context  # noqa: C901
    def configure(ctx, self, target=None, arch=None, plat=None, options=[],
                  force_configure=False):
        """
        Configure a Unikraft application.
        """

        if target is not None and isinstance(target, Target):
            arch = target.architecture
            plat = target.platform

        archs = list()
        plats = list()

        def match_arch(arch, target):
            if isinstance(arch, six.string_types) and \
                    arch == target.architecture.name:
                return target.architecture
            if isinstance(arch, Architecture) and \
                    arch.name == target.architecture.name:
                return arch
            return None

        def match_plat(plat, target):
            if isinstance(plat, six.string_types) and \
                    plat == target.platform.name:
                return target.platform
            if isinstance(plat, Platform) and \
                    plat.name == target.platform.name:
                return plat
            return None

        if len(self.config.targets.all()) == 1 \
                and target is None and arch is None and plat is None:
            target = self.config.targets.all()[0]
            archs.append(target.architecture)
            plats.append(target.platform)

        else:
            for t in self.config.targets.all():
                if match_arch(arch, t) is not None \
                        and match_plat(plat, t) is not None:
                    archs.append(t.architecture)
                    plats.append(t.platform)

        # Generate a dynamic .config to populate defconfig with based on
        # configure's parameterization.
        dotconfig = list()
        dotconfig.extend(self.config.unikraft.kconfig or [])

        for arch in archs:
            if not arch.is_downloaded:
                raise MissingComponent(arch.name)

            dotconfig.extend(arch.kconfig)
            dotconfig.append(arch.kconfig_enabled_flag)

        for plat in plats:
            if not plat.is_downloaded:
                raise MissingComponent(plat.name)

            dotconfig.extend(plat.kconfig)
            dotconfig.append(plat.kconfig_enabled_flag)

        for lib in self.config.libraries.all():
            is_downloaded = False

            if lib.is_core and self.unikraft.is_downloaded:
                is_downloaded = True
            elif lib.is_downloaded:
                is_downloaded = True

            if not is_downloaded:
                raise MissingComponent(lib.name)

            dotconfig.extend(lib.kconfig)
            dotconfig.append(lib.kconfig_enabled_flag)

            # Depending on which compartment was selected by kraft.yaml, we might
            # need to enable some more options
            options.append(lib.compartment.mechanism.driver.kconfig_opt)

        # Add any additional confguration options, and overriding existing
        # configuraton options.
        for new_opt in options:
            if new_opt is None:
                continue

            o = new_opt.split('=')
            for exist_opt in dotconfig:
                if exist_opt is None:
                    continue

                e = exist_opt.split('=')
                if o[0] == e[0]:
                    dotconfig.remove(exist_opt)
                    break
            dotconfig.append(new_opt)

        # Create a temporary file with the kconfig written to it
        fd, path = tempfile.mkstemp()

        with os.fdopen(fd, 'w+') as tmp:
            logger.debug('Using the following defconfig:')
            for line in dotconfig:
                if line is None:
                    continue
                
                logger.debug(' > ' + line)
                tmp.write(line + '\n')

        return_code = 0

        try:
            return_code = self.make([
                ('UK_DEFCONFIG=%s' % path),
                'defconfig'
            ])
        finally:
            os.remove(path)

        return return_code

    @click.pass_context
    def add_lib(ctx, self, lib=None):
        if lib is None or str(lib) == "":
            logger.warn("No library to add")
            return False

        elif isinstance(lib, six.string_types):
            _, name, _, version = break_component_naming_format(lib)
            manifests = maniest_from_name(name)

            if len(manifests) == 0:
                logger.warn("Unknown library: %s" % lib)
                return False

            for manifest in manifests:
                self.config.libraries.add(Library(
                    name=name,
                    version=version,
                    manifest=manifest,
                ))

        self.save_yaml()

        return True

    @click.pass_context
    def build(ctx, self, target=None, n_proc=0, verbose=False):
        extra = []
        if n_proc is not None and n_proc > 0:
            extra.append('-j%s' % str(n_proc))

        # Create a no-op when target is False
        if target is False:
            return

        elif target is not None:
            extra.append(target)

        return self.make(extra, verbose)

    def list_possible_mirrors(self):
        extra = []
        for lib in self.config.libraries.all():
            if not lib.is_fetched:
                for mirror in lib.origin_mirrors:
                    response = requests.head(mirror)
                    if response.status_code == 200:
                        extra.append(
                            lib.kname +
                            UNIKRAFT_LIB_MAKEFILE_URL_EXT +
                            "=" +
                            mirror
                        )
                    else:
                        logger.debug("Mirror down: %s" % mirror)
                        break
        return extra

    @click.pass_context
    def fetch(ctx, self, verbose=False):
        extra = []

        if ctx.obj.settings.fetch_prioritize_origin is False:
            extra.extend(self.list_possible_mirrors())

        extra.append('fetch')

        return self.make(extra, verbose)

    @click.pass_context
    def compartmentalize(ctx, self):
        self.find_files()
        self.find_includedirs()

        # add heap local vars replacement rule
        DSS_enabled = False
        SHSTACK_enabled = False
        FCALLS_enabled = False
        with open(os.path.join(self.localdir, ".config")) as conf:
            conf_content = conf.read()
            if 'CONFIG_LIBFLEXOS_NONE=y' in conf_content:
                # building with the fcalls backend, no need for shared data
                SHSTACK_enabled = True
                FCALLS_enabled = True
            elif 'CONFIG_LIBFLEXOS_VMEPT=y' in conf_content:
                logger.info("Building for VM/EPT")
                SHSTACK_enabled = True
            elif 'CONFIG_LIBFLEXOS_ENABLE_DSS=y' in conf_content:
                logger.info("Building with DSS enabled")
                DSS_enabled = True
            else:
                logger.info("Building with stack-to-heap variable conversion enabled")
                if 'CONFIG_LIBFLEXOS_GATE_INTELPKU_SHARED_STACKS=y' in conf_content:
                    logger.info("Note: shared stacks enabled")
                    SHSTACK_enabled = True
                else:
                    logger.info("Note: private stacks mode")

        fulldifff = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".diff"
        fulldiff = open(fulldifff, "a")

        # cleanup declared but unused compartments
        to_remove = []
        for comp in self.compartments:
            found = False
            for lib in self.libraries:
                if (lib.compartment == comp):
                    found = True
            if (not found and not comp.default):
                logger.warn("Declared compartment #" + str(comp.number) +
                            " (" + comp.name +
                            ") but no library associated with it "
                            "(ignoring it, here be dragons!)")
                to_remove.append(comp)

        largest = 0
        for comp in to_remove:
            if (comp.number > largest):
                largest = comp.number
            self.compartments.remove(comp)

        if (len(to_remove) > 0):
            for comp in self.compartments:
                if (comp.number > largest):
                    logger.error("At least compartment #" + str(largest) +
                               " is unused, while #" + str(comp.number) + " (" + comp.name +
                               ") is: such holes of unusued compartments are illegal")
                    raise KraftError('Invalid compartment list.')

        # first rewrite: make sure to create the right number of sections in the
        # linker script, that we initialize them in ukboot, etc.
        if (len(self.compartments) > 1):

            def simple_replace(template_path, path, marker, shstack_enabled=True):
                # shstack_enabled = should we replace when shared stacks are enabled?
                if SHSTACK_enabled and not shstack_enabled:
                    return
                filep = os.path.join(self._config.unikraft.localdir, path)
                comps = list(set(self.compartments) - set([c for c in self.compartments if c.default]))
                textual_replacement(comps, template_path, filep,
                        marker, fulldiff=fulldiff)

            # FIXME hardcoded paths here
            simple_replace(
                "linkerscript_data.in",
                "plat/kvm/x86/link64.lds.S",
                "/* __FLEXOS MARKER__: insert compartment data sections here. */")
            simple_replace(
                "linkerscript_bss.in",
                "plat/kvm/x86/link64.lds.S",
                "/* __FLEXOS MARKER__: insert compartment bss sections here. */")

            tmpl = ""
            if (type(self.compartments[0].mechanism.driver) == VMEPTDriver):
                tmpl = "linkerscript_initarray_ept.in",
            elif (type(self.compartments[0].mechanism.driver) == IntelPKUDriver or
                  type(self.compartments[0].mechanism.driver) == FcallsDriver):
                tmpl = "linkerscript_initarray_vanilla.in",
            else:
                logger.error("Transformations not supported for this mechanism (" +
                        str(type(self.compartments[0].mechanism.driver))+ ")")
                exit(1)
            simple_replace(
                tmpl, "plat/kvm/x86/link64.lds.S",
                "/* __FLEXOS MARKER__: insert compartment init array sections here. */")

            simple_replace(
                "ukboot_decl_sections.in",
                "lib/ukboot/boot.c",
                "/* __FLEXOS MARKER__: insert compartment sections decls here. */")
            simple_replace(
                "ukboot_init_sections.in",
                "lib/ukboot/boot.c",
                "/* __FLEXOS MARKER__: insert compartment sections initializers here. */")
            simple_replace(
                "flexos_core_alloc_decls.in",
                "lib/flexos-core/include/flexos/impl/intelpku.h",
                "/* __FLEXOS MARKER__: insert compartment allocator decls here. */")
            simple_replace(
                "ukalloc_getdefault.in",
                "lib/ukalloc/include/uk/alloc.h",
                "/* __FLEXOS MARKER__: insert compartment-specific allocator cases here. */")

            # these are related to per compartment stacks, so we don't want to
            # do these replacements if we are using the shared stack.
            simple_replace(
                "flexos_core_tsb_decls.in",
                "lib/flexos-core/intelpku.c",
                "/* __FLEXOS MARKER__: insert tsb decls here. */",
                shstack_enabled=False)
            simple_replace(
                "flexos_core_tsb_hdecls.in",
                "lib/flexos-core/include/flexos/impl/intelpku.h",
                "/* __FLEXOS MARKER__: insert tsb extern decls here. */",
                shstack_enabled=False)
            simple_replace(
                "ukthread_initcall.in",
                "lib/uksched/sched.c",
                "/* __FLEXOS MARKER__: uk_thread_init call */",
                shstack_enabled=False)
            simple_replace(
                "ukthread_initdecl.in",
                "lib/uksched/include/uk/thread.h",
                "/* __FLEXOS MARKER__: uk_thread_init decl */",
                shstack_enabled=False)
            simple_replace(
                "ukthread_initdecl.in",
                "lib/uksched/sched.c",
                "/* __FLEXOS MARKER__: uk_thread_init decl */",
                shstack_enabled=False)
            simple_replace(
                "ukthread_initdecl.in",
                "lib/uksched/thread.c",
                "/* __FLEXOS MARKER__: uk_thread_init decl */",
                shstack_enabled=False)
            simple_replace(
                "uksched_initstacks.in",
                "lib/uksched/sched.c",
                "/* __FLEXOS MARKER__: insert stack allocations here. */",
                shstack_enabled=False)
            simple_replace(
                "ukthread_installstacks.in",
                "lib/uksched/thread.c",
                "/* __FLEXOS MARKER__: insert stack installations here. */",
                shstack_enabled=False)
            simple_replace(
                "uksched_idleinit_nulls.in",
                "lib/ukschedcoop/schedcoop.c",
                "/* __FLEXOS MARKER__: uk_sched_idle_init nulls */",
                shstack_enabled=False)

        # this replacement is a little bit less standard :)
        comp_uksched = 0
        comp_vfscore = 0
        for lib in self.libraries:
            if lib.name == "uksched":
                comp_uksched = lib.compartment.number
            if lib.name == "vfscore":
                comp_vfscore = lib.compartment.number

        filep = os.path.join(self._config.unikraft.localdir, "lib/ukboot/boot.c")
        backup_src = os.path.join("/tmp", next(tempfile._get_candidate_names())) + ".bak"
        shutil.copyfile(filep, backup_src)

        with fileinput.FileInput(filep, inplace=True) as file:
            for line in file:
                print(line.replace("/* __FLEXOS MARKER__: uksched allocator */",
                                "flexos_comp" + str(comp_uksched) + "_alloc"), end='')
        with fileinput.FileInput(filep, inplace=True) as file:
            for line in file:
                print(line.replace("/* __FLEXOS MARKER__: vfscore compartment */",
                                str(comp_vfscore)), end='')

        cmd = ["diff", "-urNp", backup_src, filep]
        subprocess.call(cmd, stdout=fulldiff, stderr=subprocess.STDOUT)

        # now do library-specific rewrites
        for lib in self.libraries:
            # first add per-library linker scripts
            if (not lib.compartment.default):
                add_local_linkerscript(lib, fulldiff=fulldiff)

            # then generate cocci files dynamically from the template
            gr_rule_template = get_sec_rule("gatereplacer.cocci.in")
            if FCALLS_enabled:
                cb_rule_template = get_sec_rule("rmcallbacks.cocci.in")
            else:
                cb_rule_template = get_sec_rule("callbackreplacer.cocci.in")
            gr_rule_template = gr_rule_template.replace("{{ comp_cur_nb }}",
                str(lib.compartment.number))
            cb_rule_template = cb_rule_template.replace("{{ comp_cur_nb }}",
                str(lib.compartment.number))
            gr_rule = ""

            def gr_gen_rule(dest_name, dest_comp):
                gr_rule = str(gr_rule_template)
                name = dest_name

                if (dest_name != "lname" and not dest_name.startswith("app-")):
                    name = "lib" + name.replace("-", "")

                gr_rule = gr_rule.replace("{{ lib_dest_name }}", name)
                gr_rule = gr_rule.replace("{{ comp_dest_nb }}", str(dest_comp.number))

                gr_gate = dest_comp.mechanism.driver.gate_str

                if dest_comp == lib.compartment:
                    # FIXME magic value, put somewhere
                    gr_gate = "flexos_nop_gate"

                gr_rule = gr_rule.replace("{{ gate }}", gr_gate)
                gr_rule = gr_rule.replace("{{ gate_r }}", gr_gate + "_r")
                gr_rule = gr_rule.replace("{{ rule_nr }}", str(gr_gen_rule.rule_ctr))

                rule = gr_rule + "\n"
                gr_gen_rule.rule_ctr += 1
                return rule

            gr_gen_rule.rule_ctr = 0

            def cb_gen_rule(dest_name, dest_comp):
                cb_rule = str(cb_rule_template)
                name = dest_name.replace("-", "")

                if (dest_name != "lname" and not dest_name.startswith("app")
                                        and not dest_name.startswith("lib")):
                    name = "lib" + name

                cb_rule = cb_rule.replace("{{ lib_from_name }}", name)
                cb_rule = cb_rule.replace("{{ comp_from_nb }}", str(dest_comp.number))

                cb_gate = lib.compartment.mechanism.driver.gate_str

                if dest_comp == lib.compartment:
                    # FIXME magic value, put somewhere
                    cb_gate = "flexos_nop_gate"

                cb_rule = cb_rule.replace("{{ gate }}", cb_gate)
                cb_rule = cb_rule.replace("{{ gate_r }}", cb_gate + "_r")
                cb_rule = cb_rule.replace("{{ rule_nr }}", str(cb_gen_rule.rule_ctr))
                rule = cb_rule + "\n"

                cb_gen_rule.rule_ctr += 1
                return rule

            cb_gen_rule.rule_ctr = 0

            # FIXME FLEXOS this is a hack to make sure that we don't overwhelm
            # Coccinelle with hundreds of rules. We should be able to improve it
            # with future upstream releases of Coccinelle, talking about it with Julia
            whitelisted_libs = ["libvfscore", "libuknetdev", "newlib",
                                "libuksched", "libuksignal", "libukboot"]
            for dest_lib in self.libraries:
                if (not dest_lib.compartment.default) and (dest_lib.name != lib.name):
                    # this library is not in the default compartment, add a specific rule
                    gr_rule += gr_gen_rule(dest_lib.name, dest_lib.compartment)
                    gr_rule += cb_gen_rule(dest_lib.name, dest_lib.compartment)
                    # FIXME FLEXOS this is a hack :)
                    if (dest_lib.name == "newlib"):
                        gr_rule += cb_gen_rule("libc", dest_lib.compartment)
                if ((dest_lib.compartment.default) and (dest_lib.name != lib.name)
                        and dest_lib.name in whitelisted_libs):
                    gr_rule += cb_gen_rule(dest_lib.name, dest_lib.compartment)
                    # FIXME FLEXOS this is a hack :)
                    if (dest_lib.name == "newlib"):
                        gr_rule += cb_gen_rule("libc", dest_lib.compartment)

            # default rule, lname will match anything so this rule has to be at the end
            default_comp = [comp for comp in self.compartments if comp.default][0]
            gr_rule += gr_gen_rule("lname", default_comp)

            # add malloc/free/calloc replacement rule
            gr_rule += get_sec_rule("mallocreplacer.cocci")

            # add heap local vars replacement rule
            if FCALLS_enabled:
                gr_rule += get_sec_rule("remove-attrs.cocci")
            elif DSS_enabled:
                gr_rule += get_sec_rule("dss-localvars.cocci")
            else:
                # FIXME this is a terrible hack, but hey, we don't have time
                # for some reason rules are applied only once by Coccinelle,
                # so, if we don't repeat it, it doesn't get applied.
                # 20 "ought to be enough for anybody" haha.
                template = get_sec_rule("heap-localvars-1.cocci.in")
                for i in range(15):
                    gr_rule += template.replace("{{ rule_nr }}", str(i)) + "\n"

                # Est-il jour ? Est-il nuit ? horreur crépusculaire !
                template = get_sec_rule("heap-localvars-2.cocci.in")
                for i in range(15):
                    gr_rule += template.replace("{{ rule_nr }}", str(i)) + "\n"

            # add global vars replacement rule
            gr_rule += get_sec_rule("globalvars.cocci")

            # write rule somewhere so that Coccinelle can access it
            rule_file = os.path.join("/tmp", next(tempfile._get_candidate_names()) + ".cocci")
            with open(rule_file, "w") as f:
                f.write(gr_rule)
            logger.debug("Coccinelle rules for " + lib.name + ": " + rule_file)

            coccinelle_rewrite(lib, rule_file, fulldiff)

        logger.info("Full diff of rewritings: %s" % fulldifff)
        fulldiff.close()

    @click.pass_context
    def prepare(ctx, self, verbose=False):
        return self.make('prepare', verbose)

    def init(self, create_makefile=False, force_create=False):
        """
        Initialize an app component's directory.
        """
        makefile_uk = os.path.join(self.localdir, MAKEFILE_UK)
        if os.path.exists(makefile_uk) is False or force_create:
            logger.debug("Creating: %s" % makefile_uk)
            Path(makefile_uk).touch()

        if create_makefile:
            pass

        try:
            filenames = get_default_config_files(self.localdir)
        except KraftFileNotFound:
            filenames = []

        if len(filenames) == 0 or force_create:
            self.save_yaml()

    def run(self, target=None, initrd=None, background=False,  # noqa: C901
            paused=False, gdb=4123, dbg=False, virtio_nic=None, bridge=None,
            interface=None, dry_run=False, args=None, memory=64, cpu_sockets=1,
            cpu_cores=1):

        if target is None:
            raise KraftError('Target not set')

        elif target.binary is None:
            raise KraftError('Target has not been compiled')

        elif not os.path.exists(target.binary):
            raise KraftError('Could not find unikernel: %s' % target.binary)

        logger.debug("Running binary: %s" % target.binary)

        runner = target.platform.runner
        runner.use_debug = dbg
        runner.architecture = target.architecture.name

        if initrd:
            runner.add_initrd(initrd)

        if virtio_nic:
            runner.add_virtio_nic(virtio_nic)

        if bridge:
            runner.add_bridge(bridge)

        if interface:
            runner.add_interface(interface)

        if gdb:
            runner.open_gdb(gdb)

        if memory:
            runner.set_memory(memory)

        if cpu_sockets:
            runner.set_cpu_sockets(cpu_sockets)

        if cpu_cores:
            runner.set_cpu_cores(cpu_cores)

        runner.unikernel = target.binary
        runner.execute(
            extra_args=args,
            background=background,
            paused=paused,
            dry_run=dry_run,
        )

    def clean(self, proper=False):
        """
        Clean the application.
        """

        if proper:
            self.make("properclean")

        else:
            self.make("clean")

    @property
    def binaries(self):
        binaries = []

        for target in self.config.targets.all():
            if target.binary is not None:
                binaries.append(target)

        return binaries

    def repr(self):
        return self.config

    def save_yaml(self, file=None):
        if file is None:
            file = os.path.join(self.localdir, SUPPORTED_FILENAMES[0])

        yaml = serialize_config(
            self.repr(),
            original=file
        )

        logger.debug("Saving: %s" % file)

        with open(file, 'w+') as f:
            f.write(yaml)
