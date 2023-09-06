#Edited by G Walters NWRFC 10/13/2021 
#Used to convert CHPS XML to pandas dataframe

from collections import OrderedDict
from lxml import etree
import pandas as pd
import warnings

def read_pixml_timeseries(f):
    """Reads a pixml timeseries file from disk

    Args:
        f: A string representing the path to the file to read

    Returns:
        The appropriate pixml class type
    """

    tree = etree.parse(f)
    root = tree.getroot()

    return PixmlTimeSeries(root)


def timeseries_to_csv(input_file, output_file, header_elements):
    """Converts a pixml timeseries to a long csv format

    Args:
        input_file: A string representing the full path to pixml timeseries
        output_file: A string represnting where to write the output csv
        header_elements: A list of header elements to include in output csv from
                         pixml timeseries

    Returns:
        None
    """

    pixml_ts = read_pixml_timeseries(input_file)
    df = pixml_ts.to_df(header_elements)
    df.to_csv(output_file, index=False)


class PixmlTimeSeries(object):
    """Object containing PI TimeSeries xml data

    Attributes:
        namespace:
        xml:
    """

    namespace = '{http://www.wldelft.nl/fews/PI}'
    

    def __init__(self, xml):
        self.xml = xml

        self._header_processor = {
            'locationId': self._process_header_text,
            'parameterId': self._process_header_text,
            'qualifierId': self._process_qualifiers_text,
            'units': self._process_header_text,
            'forecastDate': self._process_header_date,
            'creationDate': self._process_header_text,
            'creationTime': self._process_header_text
        }

    def to_df(self, header_elements):
        """
        Converts xml to a pandas dataframe in long format

        Args:
            header_elements: A list of valid header elements to extract

        Returns:
            A pandas dataframe in long format
        """
        
        #TODO: Need to handle cases when a pixml file contains variable header elements

        all_series = self.xml.findall('.//{}series'.format(self.namespace))

        rows = []
        for series in all_series:
            header=series.find('.//{}header'.format(self.namespace))

            # keep only header elements which are contained in series header
            series_header =  self._verify_elements(header, header_elements)

            events = series.findall('.//{}event'.format(self.namespace))
            if len(events) == 0:
                continue
            
            metadata = OrderedDict(
                (header_element, self._header_processor[header_element](header, header_element))
                for header_element in series_header)

            for event in events:
                event_data = self._process_event(event)
                rows.append(list(metadata.values()) + event_data)

        if len(rows) == 0:
            return pd.DataFrame()

        columns = list(metadata.keys()) + ['valid_datetime', 'value']
        return pd.DataFrame(rows, columns=columns)

    def _process_qualifiers_text(self, h, he):
        qualifiers = h.findall('.//{}{}'.format(self.namespace, he))
        return ','.join([qualifier.text for qualifier in qualifiers])

    def _process_header_date(self, h, he):
        date_element = h.find('.//{}{}'.format(self.namespace, he))
        return '{} {}'.format(date_element.attrib['date'], date_element.attrib['time'])

    def _process_header_text(self, h, he):
        return h.findtext('.//{}{}'.format(self.namespace, he))
        
    def _process_event(self, e):
        dt = '{} {}'.format(e.attrib['date'], e.attrib['time'])
        value = e.attrib['value']
        return [dt, value]

    def _has_elements(self, h, hes):
        for he in hes:
            if h.find('.//{}{}'.format(self.namespace, he)) is None:
                warnings.warn('{} is missing from {}'.format(he, etree.tostring(h)))
                return False
        return True
    
    def _verify_elements(self, h, hes):
        return [he for he in hes if h.find('.//{}{}'.format(self.namespace, he)) is not None]