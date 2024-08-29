from __future__ import absolute_import
from __future__ import unicode_literals

from .driver import Driver


class MTEDriver(Driver):
    # MTE backend has a color tag so save it here and retrieve it in the superclass
    _color_tag = None
    @property
    def color(self) : return self._color_tag
    
    gate_str = "flexos_mte_gate"
    kconfig_opt = "CONFIG_LIBFLEXOS_MTE=y"
    pass