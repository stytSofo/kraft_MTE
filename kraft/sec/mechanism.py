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

from .driver import CompartmentDriverTypes

from kraft.error import KraftError


class Mechanism(object):
    _config = None

    @property
    def config(self): return self._config

    _driver = None

    @property
    def driver(self): return self._driver

    def __init__(self, *args, **kwargs):
        self._config = kwargs

        mechanism = kwargs.get('mechanism', None)
       
        if mechanism is not None and isinstance(mechanism, six.string_types):
            driver = CompartmentDriverTypes.from_name(
                name=mechanism
            )
            if driver is not None:
                self._driver = driver.cls(
                    name=mechanism
                )
        
        elif 'driver' in self.config:
            driver = CompartmentDriverTypes.from_name(
                name=self.config['driver']
            )
            if driver is not None:
                self._driver = driver.cls(
                    name=self.config['driver'],
                    **self.config
                )

        if self._driver is None:
            raise KraftError("Unspecified mechanism driver")

    def repr(self):
        config = {
            'driver': self.driver.name
        }

        config = {**config, **self.driver.repr()}

        return config

