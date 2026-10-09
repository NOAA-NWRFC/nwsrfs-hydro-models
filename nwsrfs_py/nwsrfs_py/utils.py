import os, numbers
from dataclasses import is_dataclass, fields
from importlib import resources
from contextlib import contextmanager
from pathlib import Path
from typing import Any, List
import pandas as pd
import numpy as np

def _validate_nd_array(*args: np.ndarray, ndim: int = 1):
    """
    Check whether all provided NumPy arrays match the expected dimension.

    Args:
        ndim (int): Expected number of dimensions (e.g., 1, 2, 3)
        *args (np.ndarray): Numpy arrays to check shape.
    Returns:
        bool: Returns ``True`` if all the numpy array shapes are 1d, and returns ``False``
        if any of the numpy arrays are not.
    """
    for arg in args:

        if not isinstance(arg, np.ndarray):
            raise TypeError(f"Expected np.ndarray, got {type(arg).__name__}")
        if arg.ndim != ndim:
            return False
    return True

def _validate_array_length(arg1:np.ndarray, *args:np.ndarray):
    """
    Check to make sure all the numpy arrays have the same length. This function is flexible to compare multiple
    arrays.

    Args:
        arg1 (np.ndarray): The reference numpy array.
        *args (np.ndarray): Numpy arrays to compare length.
    Returns:
        bool: Returns ``True`` if all the numpy array lengths are the same, and returns ``False``
        if any of the numpy arrays are not.
    """
    check_len = len(arg1)
    for arg in args:
        if not check_len == len(arg):
            return False
    return True

def _validate_positive_values(*args: np.ndarray, allow_zero: bool = True):
    """
    Check to make sure all the numpy arrays have only positive values. This function is flexible to check multiple
    arrays.

    Args:
        *args (np.ndarray): Numpy arrays to check for positive values.
        allow_zero (bool, optional): If ``True`` (default), values must be non-negative (>= 0).
            If ``False``, values must be strictly positive (> 0).
    Returns:
        bool: ``True`` if all elements in all arrays meet the condition; ``False`` otherwise.s.
    """
    for arg in args:
        if not isinstance(arg, np.ndarray):
            raise TypeError(f"Expected np.ndarray, got {type(arg).__name__}")

        # Choose operator based on allow_zero flag
        is_valid = (arg >= 0) if allow_zero else (arg > 0)

        if not np.all(is_valid):
            return False

    return True

def _datetime_conversion(year,month,day,hour=0):
    """
    Converts time arrays to a datetime datatype within a pandas Series. This function is flexible to handle daily or 
    subdaily time arrays data.

    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray | int): Array of hours corresponding to each timestep (units: time). Default: 0.
    Returns:
        pd.Series: Returns Series with a column named `datetime` with the time array converted 
        to datetime format.
    """
    df_datetime = pd.DataFrame({'year':year,'month':month,'day':day,'hour':hour})

    return pd.to_datetime(df_datetime).rename('datetime')

def _validate_timestep(dt_sec: int, year: np.ndarray, month: np.ndarray, 
                        day: np.ndarray, hour: np.ndarray | int = 0):
    """
    Converts time arrays to a datetime datatype and checks to make sure all time steps associated with the datetime match ``dt_sec``.
    
    Args:
        dt_sec (int): Timestep to verify in seconds (units: time).
        year (np.ndarray): Array of years for each timestep.
        month (np.ndarray): Array of months corresponding to each timestep.
        day (np.ndarray): Array of days for each timestep.
        hour (np.ndarray | int): Array of hours corresponding to each timestep. Default: 0.
    Returns:
        bool: Returns ``True`` if all the datetime timesteps are equal to ``dt_sec``, and returns ``False``
        if any of the timesteps do not equal ``dt_sec``.
    """
    df_datetime_check = _datetime_conversion(year,month,day,hour)

    # [1:] ignores the first NaT result from the diff
    datetime_check = df_datetime_check.diff().dt.total_seconds()[1:].astype(int)

    if not (datetime_check == dt_sec).all():
        return False

    return True

def _validate_finite_fields(instance):
    """
    Check that every supplied dataclass field contains finite values.

    Fields set to None are skipped to support optional parameters.

    Args:
        instance: Dataclass instance containing numeric inputs.

    Raises:
        TypeError: If instance is not a dataclass instance or a field
            contains unsupported nonnumeric values.
        ValueError: If a field contains NaN or infinity.
    """
    if isinstance(instance, type) or not is_dataclass(instance):
        raise TypeError("Expected a dataclass instance")

    for field in fields(instance):
        value = getattr(instance, field.name)

        if value is None:
            continue

        try:
            finite = np.isfinite(value)
        except TypeError as exc:
            raise TypeError(
                f"{field.name} must contain numeric values"
            ) from exc

        if not np.all(finite):
            raise ValueError(
                f"{field.name} must contain only finite values"
            )
            

def _define_timestep_sec(year: np.ndarray, month: np.ndarray,
                        day: np.ndarray, hour: np.ndarray | int = 0):
    """
    Converts time arrays to a datetime datatype and calculates the average timestep.

    Args:
        year (np.ndarray): Array of years for each timestep.
        month (np.ndarray): Array of months corresponding to each timestep.
        day (np.ndarray): Array of days for each timestep.
        hour (np.ndarray | int): Array of hours corresponding to each timestep. Default: 0.
    Returns:
        int: Returns the average timestep in seconds.
    """
    df_datetime = _datetime_conversion(year,month,day,hour)
    # [1:] ignores the first NaT result from the diff
    datetime_delta = df_datetime.diff().dt.total_seconds()[1:]
    return int(datetime_delta.mean())

def _dtype_conversion(arg:  np.ndarray, new_dtype: type):
    """
    Converts the values to the dtype specified by ``new_dtype``.

    Args:
        arg (np.ndarray | numbers.Number): Array or number to convert to new dtype.
        new_dtype (type): The dtype to convert to.
    Returns:
        np.ndarray | numbers.Number: Returns a new numpy array or scalar with values converted to ``new_dtype``.
    """
    try:
        if isinstance(arg, np.ndarray):
            return arg.astype(new_dtype)
        else:
            return new_dtype(arg)
    except TypeError as e:
        print(f"Error: Invalid dtype specified. {e}")
        raise
    except Exception as e:
        print(f"An unexpected error occurred during conversion: {e}")
        raise

def _dtype_conversion_batch(arg,  new_dtype: type, batch: list):
    """
    Batch conversion of values to a new datatype. The values are fields in a dataclass.

    Args:
        arg (object): Dataclass instance with fields listed in ``batch``.
        new_dtype (type): The dtype to convert to.
        batch (list): A list of field names within the dataclass.
    """
    if not is_dataclass(arg):
        raise TypeError(f"Expected a dataclass instance, got {type(arg).__name__}")
    
    dc_field_names = {x.name for x in fields(arg)} 
    
    #Check if the batch_field are all in the dataclass
    if not set(batch).issubset(dc_field_names):
        raise AttributeError(f"one or more of the field in the batch list are not in the dataclass")
    
    for field in batch:
        current_attr = getattr(arg, field)
        new_attr = _dtype_conversion(current_attr, new_dtype)
        #Update the dtype
        setattr(arg, field, new_attr)

def _add_dimension_batch(arg, batch: list):
    """
    Batch conversion of 1D :class:`numpy.ndarray` to 2D :class:`numpy.ndarray`.

    Args:
        arg (object): Dataclass instance with fields listed in ``batch``.
        batch (list): A list of field names within the dataclass that are :class:`numpy.ndarray`.
    """
    if not is_dataclass(arg):
        raise TypeError(f"Expected a dataclass instance, got {type(arg).__name__}")
    
    dc_field_names = {x.name for x in fields(arg)} 
    
    #Check if the batch_field are all in the dataclass
    if not set(batch).issubset(dc_field_names):
        raise AttributeError(f"one or more of the field in the batch list are not in the dataclass")
    
    for field in batch:
        current_attr = getattr(arg, field)
        
        if not isinstance(current_attr, np.ndarray):
            raise TypeError(f"Field '{field}' must be a np.ndarray, got {type(current_attr).__name__}")
        
        if current_attr.ndim == 1:
            setattr(arg, field, current_attr[:, np.newaxis])
        elif current_attr.ndim != 2:
            raise ValueError(f"Field '{field}' has ndim={current_attr.ndim}, expected 1 or 2")

def _arrayasfortran(arg):
    """
    Batch conversion of all array fields to a FORTRAN friendly format.

    Args:
        arg (object): Dataclass instance to perform conversion on.
    """
    if not is_dataclass(arg):
    	raise TypeError(f"Expected a dataclass instance, got {type(arg).__name__}")

    for field in fields(arg):
    	current_attr = getattr(arg, field.name)
    	
    	if np.isscalar(current_attr) or current_attr is None:
    		continue

    	try: 
    		new_attr = np.asfortranarray(current_attr)
    		setattr(arg, field.name, new_attr)
    	except Exception:
    		continue

@contextmanager
def _get_example_dir(subfolder):
    """
    Returns the absolute path to a bundled data directory.
    Example: get_example_dir("NRKW1") returns the path to nwsrfs_py/data/NRKW1
    """
    # We target the __init__.py within the subfolder to find the directory
    pkg = f"nwsrfs_py.data.{subfolder}"
    init_file = resources.files(pkg) / "__init__.py"
    with resources.as_file(init_file) as p:
        yield str(Path(p).parent)

