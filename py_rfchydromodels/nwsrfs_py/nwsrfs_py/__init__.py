"""Top-level package exports for nwsrfs_py."""

__all__ = ['nwsrfs']


def __getattr__(name):
    if name == 'nwsrfs':
        from .wrapper import nwsrfs_f2py_wrapper as nwsrfs_module
        return nwsrfs_module
    raise AttributeError(f'module {__name__!r} has no attribute {name!r}')
