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

import six

from .mechanism import Mechanism

from kraft.component import ComponentManager

from itertools import count

from kraft.logger import logger

class Compartment(object):
    _compartments = count(0)

    _name = None
    
    @property
    def name(self): return self._name

    _mechanism = None
    
    @property
    def mechanism(self): return self._mechanism

    _default = False
    
    @property
    def default(self): return self._default

    def __init__(self, *args, **kwargs):
        self._config = kwargs
        self._name = kwargs.get('name', None)
        self._default = kwargs.get('default', False)

        # compartment "number"
        self.number = next(self._compartments)
        
        mechanism = kwargs.get('mechanism', None)
        if mechanism is not None and isinstance(mechanism, six.string_types):
            self._mechanism = Mechanism(
                mechanism=mechanism
            )
        elif mechanism is not None and isinstance(mechanism, dict):
            self._mechanism = Mechanism(**mechanism)

    def repr(self):
        ret = {}

        if self.name is not None:
            ret['name'] = self.name
        if self.mechanism is not None:
            ret['mechanism'] = self.mechanism.repr()
        if self.default is True:
            ret['default'] = True

        return ret


class CompartmentManager(ComponentManager):
    def __init__(self, components=[]):
        mechs = set([comp['mechanism']['driver'] for comp in components])
        if (len(mechs) > 1):
            logger.error("More than one mechanism employed (" + str(mechs) + "): illegal for now")
            exit(1)
        super(CompartmentManager, self).__init__(
            components=components,
            cls=Compartment
        )

    def set(self, k, v):
        if k is not None:
            self._components[k] = v

    def repr(self):
        ret = []
        for k in self.all():
            ret.append(k.repr())
        return ret
