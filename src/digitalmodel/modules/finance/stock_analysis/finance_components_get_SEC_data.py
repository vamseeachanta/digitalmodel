import logging
import re
import time
import traceback
import xml.etree.ElementTree as ET

import lxml
import pandas as pd
import xmltodict
from sec_edgar_downloader import Downloader
from sec_edgar_downloader._utils import get_filing_urls_to_download

from common.data import ReadDataFromString, ReadURLData, RegEx

re_methods = RegEx()

SEC_EDGAR_RATE_LIMIT_SLEEP_INTERVAL = 0.2
read_string = ReadDataFromString()

sec_http_user_agent = {
    'User-Agent': 'Vamsee Achanta support@aceengineer.com',
    'Accept-Encoding': 'gzip, deflate',
    'Host': 'www.sec.gov'
}
read_url = ReadURLData(cfg={'headers': sec_http_user_agent})


class SECDataTicker():

    def __init__(self):
        pass

    def get_company_tickers_exchange(self):
        url = 'https://www.sec.gov/files/company_tickers_exchange.json'
        cfg = {'headers': sec_http_user_agent, 'url': url}
        data = read_url.get_response_data(cfg)
        time.sleep(SEC_EDGAR_RATE_LIMIT_SLEEP_INTERVAL)
        return data

    def get_company_tickers_mf(self):
        url = 'https://www.sec.gov/files/company_tickers_mf.json'
        cfg = {'headers': sec_http_user_agent, 'url': url}
        data = read_url.get_response_data(cfg)
        time.sleep(SEC_EDGAR_RATE_LIMIT_SLEEP_INTERVAL)
        return data

    def get_company_tickers(self):
        url = 'https://www.sec.gov/files/company_tickers.json'
        cfg = {'headers': sec_http_user_agent, 'url': url}
        data = read_url.get_response_data(cfg)
        time.sleep(SEC_EDGAR_RATE_LIMIT_SLEEP_INTERVAL)
        return data


class SECDataForm():

    def __init__(self):
        self.sec_analysis_filing_types = ['10-K', '10-Q', '3', '4', '8-K', 'SC 13D', 'SC 13G', '13F-HR', '13F-NT']

    def get_sec_form_data(self, ticker, cfg_sec):
        filing_type = cfg_sec.get('filing_type')
        include_amends = cfg_sec.get('include_amends')
        after_date = cfg_sec.get('after_date')
        before_date = cfg_sec.get('before_date')
        num_filings_to_download = cfg_sec.get('num_filings_to_download')

        sec_form_columns = self.get_columns_by_filing_type(filing_type)
        sec_form_data = pd.DataFrame(columns=sec_form_columns)

        try:
            filings_to_fetch = get_filing_urls_to_download(filing_type,
                                                           ticker,
                                                           num_filings_to_download=num_filings_to_download,
                                                           after_date=after_date,
                                                           before_date=before_date,
                                                           include_amends=include_amends)

            for filing in filings_to_fetch:
                time.sleep(SEC_EDGAR_RATE_LIMIT_SLEEP_INTERVAL)

                url = self.get_form_url_by_filing_type(filing_type, filing)
                cfg_temp = {'url': url}
                response_data = read_url.get_response_data(cfg_temp)
                try:
                    if filing_type == "4":
                        result_array = self.parse_form_4(response_data)
                        sec_form_data.Date = pd.to_datetime(sec_form_data.Date, infer_datetime_format=True)
                        sec_form_data.sort_values(by='Date', ascending=True, inplace=True)
                        if result_array[-1] is not None:
                            sec_form_data.loc[len(sec_form_data)] = result_array
                    if filing_type == '13F-HR':
                        result = self.parse_form_13F_HR(response_data)
                        sec_form_data = result
                    if filing_type == '13F-NT':
                        result = self.parse_form_13F_NT(response_data)
                        sec_form_data = result
                    if filing_type == 'SC 13G':
                        result = self.parse_form_SC_13G(response_data)
                        result.update(cfg_temp)
                        sec_form_data = result

                    if filing_type == 'SC 13D':
                        result = self.parse_form_SC_13D(response_data)
                        result.update(cfg_temp)
                        sec_form_data = result
                except Exception as e:
                    print("Failed parsing URL {0} for ticker {1}".format(url, ticker))

            logging.info("{0}".format({'sec_data_' + filing_type: True}))
        except:
            logging.debug(f"Unable to obtain SEC form data for filing type: {filing_type}".format(filing_type))
            print(f"Unable to obtain SEC form data for filing type: {filing_type}".format(filing_type))
            logging.info("StockAnalysis Data Error: (%s)" % traceback.format_exc())
            print("StockAnalysis Data Error: (%s)" % traceback.format_exc())
            logging.info("{0}".format({'sec_data_' + filing_type: False}))

        return sec_form_data

    def get_valid_forms(self, ticker):
        dl = Downloader()
        ticker_valid_filing_types = []
        for filing_type in self.sec_analysis_filing_types:
            filings_to_fetch = get_filing_urls_to_download(filing_type,
                                                           ticker,
                                                           num_filings_to_download=1,
                                                           after_date=None,
                                                           before_date=None,
                                                           include_amends=True)
            if len(filings_to_fetch) > 0:
                ticker_valid_filing_types.append(filing_type)

        return ticker_valid_filing_types

    def get_cusip(self, ticker, valid_sec_forms):
        cusip = None
        status = {}
        sec_form_data = {}
        cfg_sec = {}
        if 'SC 13G' in valid_sec_forms:
            cfg_sec = {
                'filing_type': 'SC 13G',
                'num_filings_to_download': 1,
                'include_amends': False,
                'after_date': None,
                'before_date': None
            }

            sec_form_data = self.get_sec_form_data(ticker, cfg_sec)
            cusip_url = sec_form_data.get('url', None)
            logging.debug(f"cusip_url: {cusip_url}".format(cusip_url))
            lines_with_cusip = sec_form_data.get('cusip', [])
            if len(lines_with_cusip) > 0:
                cusip = self.get_cusip_from_string(cusip, lines_with_cusip)
        elif (cusip is None) and ('SC 13D' in valid_sec_forms):
            cfg_sec = {
                'filing_type': 'SC 13D',
                'num_filings_to_download': 1,
                'include_amends': False,
                'after_date': None,
                'before_date': None
            }
            sec_form_data = self.get_sec_form_data(ticker, cfg_sec)
            cusip_url = sec_form_data.get('url', None)
            logging.debug(f"cusip_url: {cusip_url}".format(cusip_url))
            lines_with_cusip = sec_form_data.get('cusip', [])
            if len(lines_with_cusip) > 0:
                cusip = self.get_cusip_from_string(cusip, lines_with_cusip)

        status.update({'cusip_url': sec_form_data.get('url', None), 'cusip_form': cfg_sec.get('filing_type', None)})

        return cusip, status

    def get_cusip_from_string(self, cusip, lines_with_cusip):
        pattern_list = [r'CUSIP Number:', r'CUSIP No.']

        for line_with_cusip in lines_with_cusip:
            for pattern in pattern_list:
                if pattern in line_with_cusip:
                    cusip_array = re.findall(pattern + '(.*)', line_with_cusip)[0].split(' ')
                    for item in cusip_array:
                        if len(item) == 9:
                            cusip = item
                            return cusip

    def get_columns_by_filing_type(self, filing_type):
        if filing_type == 4:
            columns = [
                'trasactionType', 'Insider Trading', 'Relationship', 'Date', 'Transaction', 'Cost', '#Shares',
                'Value ($)', '#Shares Total', 'SEC Form 4', 'Insider_id', 'ShareRatio', 'start_shares',
                'share_holding_ratio', 'average_cost'
            ]
        else:
            columns = None

        return columns

    def get_form_url_by_filing_type(self, filing_type, filing):
        url = None
        if filing_type == "4":
            url = filing.filing_details_url
        elif filing_type == '13F-HR':
            url = filing.full_submission_url
        elif filing_type == '13F-NT':
            url = filing.full_submission_url
        elif filing_type == 'SC 13D':
            url = filing.full_submission_url
        elif filing_type == 'SC 13G':
            url = filing.full_submission_url

        return url

    def parse_form_13F_HR(self, data):
        dict_obj_arrays = read_string.get_xml_dict_from_byte_data(data)

        infoTable = dict_obj_arrays[1]['informationTable']['infoTable']
        infoTable_df = pd.DataFrame(infoTable)
        json_obj = infoTable_df.to_json(orient='records')

        try:
            reportCalendarOrQuarter = dict_obj_arrays[0]['edgarSubmission']['formData']['coverPage'][
                'reportCalendarOrQuarter']
        except:
            reportCalendarOrQuarter = None
        try:
            signatureDate = dict_obj_arrays[0]['edgarSubmission']['formData']['signatureBlock']['signatureDate']
        except:
            signatureDate = None

        result = {
            'infoTable': json_obj,
            'reportCalendarOrQuarter': reportCalendarOrQuarter,
            'signatureDate': signatureDate
        }

        return result

    def parse_form_13F_NT(self, data):
        dict_obj_arrays = read_string.get_xml_dict_from_byte_data(data)

        infoTable = dict_obj_arrays[1]['informationTable']['infoTable']
        infoTable_df = pd.DataFrame(infoTable)
        json_obj = infoTable_df.to_json(orient='records')

        try:
            reportCalendarOrQuarter = dict_obj_arrays[0]['edgarSubmission']['formData']['coverPage'][
                'reportCalendarOrQuarter']
        except:
            reportCalendarOrQuarter = None
        try:
            signatureDate = dict_obj_arrays[0]['edgarSubmission']['formData']['signatureBlock']['signatureDate']
        except:
            signatureDate = None

        result = {
            'infoTable': json_obj,
            'reportCalendarOrQuarter': reportCalendarOrQuarter,
            'signatureDate': signatureDate
        }

        return result

    def parse_form_SC_13D(self, data_byte):
        lines_with_cusip_keywords = self.prepare_SC_13_form_for_cusip_extraction(data_byte)

        result = {'cusip': lines_with_cusip_keywords}
        return result

    def parse_form_SC_13G(self, data_byte):
        lines_with_cusip_keywords = self.prepare_SC_13_form_for_cusip_extraction(data_byte)

        result = {'cusip': lines_with_cusip_keywords}
        return result

    def prepare_SC_13_form_for_cusip_extraction(self, data_byte):
        cfg_temp = {'data_byte': data_byte, 'features': 'lxml'}
        text_without_html_tags = re_methods.get_without_html_tags(cfg_temp)
        pattern = [r'[^\x00-\x7F]+']
        replace_with = [' ']
        cfg_temp = {'data_string': text_without_html_tags, 'pattern': pattern, 'replace_with': replace_with}
        text_without_spaces = re_methods.replace_in_string(cfg_temp)
        pattern = '/n'
        replace_with = '\n'
        cfg_temp = {'data_string': text_without_spaces, 'pattern': pattern, 'replace_with': replace_with}
        clean_text_string = re_methods.replace_in_string(cfg_temp)
        pattern = [r'CUSIP No.(\s*)', r'CUSIP(\s*)No.(\s*)', r'CUSIP NO.(\s*)', r'CUSIP(\s*)NO.(\s*)']
        replace_with = r'CUSIP No. '
        cfg_temp = {'data_string': clean_text_string, 'pattern': pattern, 'replace_with': replace_with}
        clean_text_string_key_words_1 = re_methods.replace_in_string(cfg_temp)
        pattern = [r'CUSIP Number:(\s*)', r'CUSIP(\s*)Number:(\s*)', r'CUSIP NUMBER:(\s*)', r'CUSIP(\s*)NUMBER:(\s*)']
        replace_with = r'CUSIP Number: '
        cfg_temp = {'data_string': clean_text_string_key_words_1, 'pattern': pattern, 'replace_with': replace_with}
        clean_text_string_key_words_2 = re_methods.replace_in_string(cfg_temp)
        key_words = ['CUSIP No.', 'CUSIP Number:']
        cfg_temp = {
            'data_string': clean_text_string_key_words_2,
            'line': {
                'key_words': key_words,
                'transform': {
                    'scale': 1,
                    'shift': 0
                }
            }
        }
        lines_with_keywords = read_string.get_all_lines_containing_any_key_words(cfg_temp)

        return lines_with_keywords

    def parse_form_4(self, response_data):
        data = xmltodict.parse(response_data)
        if data is not None:

            Owner, Relationship = self.get_owner_info(data['ownershipDocument']['reportingOwner'])
            trasactionType, Cost, Transaction, share_ratio, total_shares, noShares = self.get_transaction_info(
                data['ownershipDocument'])
            Date = data['ownershipDocument']['periodOfReport']

            result_array = [
                trasactionType, Owner, Relationship, Date, Transaction, Cost, noShares, None, total_shares, None, None,
                share_ratio
            ]
            return result_array

    def get_owner_info(self, owner_data):
        if type(owner_data) is list:
            Owner = [item['reportingOwnerId']['rptOwnerName'] for item in owner_data]
            Owner = ' ; '.join(Owner)
            # TODO expand to entire array instead of first item
            Relationship = self.get_Relationship(owner_data[0])
        else:
            Owner = owner_data['reportingOwnerId']['rptOwnerName']
            Relationship = self.get_Relationship(owner_data)

        return Owner, Relationship

    def get_Relationship(self, owner_data):
        reportingOwnerRelationship = owner_data['reportingOwnerRelationship']
        officerTitle = None
        officerTitle = reportingOwnerRelationship.get('officerTitle', None)
        # officerTitle = [item['reportingOwnerRelationship']['officerTitle'] for item in owner_data]
        if officerTitle is not None:
            Relationship = officerTitle
        elif ('isDirector' in reportingOwnerRelationship) and (reportingOwnerRelationship['isDirector'] == '1' or
                                                               reportingOwnerRelationship['isDirector']):
            Relationship = 'Director'
        elif ('isOfficer' in reportingOwnerRelationship) and (reportingOwnerRelationship['isOfficer'] == '1' or
                                                              reportingOwnerRelationship['isOfficer']):
            Relationship = 'Officer'
        elif ('isTenPercentOwner'
              in reportingOwnerRelationship) and (reportingOwnerRelationship['isTenPercentOwner'] == '1' or
                                                  reportingOwnerRelationship['isTenPercentOwner']):
            Relationship = 'TenPercentOwner'
        elif ('isOther' in reportingOwnerRelationship) and (reportingOwnerRelationship['isOther'] == '1' or
                                                            reportingOwnerRelationship['isOther']):
            Relationship = 'Other'
        else:
            Relationship = None

        return Relationship

    def get_transaction_info(self, transaction_data):
        if 'derivativeTable' in transaction_data and transaction_data['derivativeTable'] is not None:
            Cost, Transaction, share_ratio, total_shares, noShares = self.get_derivativeTrasactionTable(
                transaction_data['derivativeTable'])
            return 'derivativeTable', Cost, Transaction, share_ratio, total_shares, noShares

        if 'nonDerivativeTable' in transaction_data and transaction_data['nonDerivativeTable'] is not None:
            Cost, Transaction, share_ratio, total_shares, noShares = self.get_nonDerivativeTrasactionTable(
                transaction_data['nonDerivativeTable'])
            return 'nonDerivativeTable', Cost, Transaction, share_ratio, total_shares, noShares

    def get_nonDerivativeTrasactionTable(self, table_data):
        import pandas as pd

        if type(table_data['nonDerivativeTransaction']) is not list:
            table_data['nonDerivativeTransaction'] = [table_data['nonDerivativeTransaction']]
        table_df = pd.DataFrame(table_data['nonDerivativeTransaction'])

        Cost, Transaction, share_ratio, total_shares, noShares = self.get_trasactionTable(table_df)
        return Cost, Transaction, share_ratio, total_shares, noShares

    def get_trasactionTable(self, table_df):
        table_df['securityTitle'] = [item['value'] for item in table_df['securityTitle'].to_list()]
        table_df['transactionDate'] = [item['value'] for item in table_df['transactionDate'].to_list()]
        table_df['noShares'] = [
            float(item['transactionShares']['value']) for item in table_df['transactionAmounts'].to_list()
        ]
        table_df['transactionPricePerShare'] = [
            float(item['transactionPricePerShare']['value'])
            if 'value' in item['transactionPricePerShare'].keys() else None
            for item in table_df['transactionAmounts'].to_list()
        ]

        if table_df['transactionPricePerShare'].max() == 0:
            table_df['transactionPricePerShare'] = [
                float(item['value']) if 'value' in item.keys() else None
                for item in table_df['conversionOrExercisePrice'].to_list()
            ]

        table_df['transactionAcquiredDisposedCode'] = [
            item['transactionAcquiredDisposedCode']['value'] for item in table_df['transactionAmounts'].to_list()
        ]
        table_df['postTransactionAmounts'] = [
            float(item['sharesOwnedFollowingTransaction']['value'])
            if 'value' in item['sharesOwnedFollowingTransaction'].keys() else None
            for item in table_df['postTransactionAmounts'].to_list()
        ]
        table_df['preTransactionAmounts'] = None
        noShares = 0
        for row_index in range(0, len(table_df)):
            if table_df['transactionAcquiredDisposedCode'].iloc[row_index] == 'A':
                table_df['preTransactionAmounts'].iloc[row_index] = table_df['postTransactionAmounts'].iloc[
                    row_index] - table_df['noShares'].iloc[row_index]
                noShares = noShares - table_df['noShares'].iloc[row_index]
            else:
                table_df['preTransactionAmounts'].iloc[row_index] = table_df['postTransactionAmounts'].iloc[
                    row_index] + table_df['noShares'].iloc[row_index]
                noShares = noShares + table_df['noShares'].iloc[row_index]
        if table_df['preTransactionAmounts'].iloc[0] > 0:
            share_ratio = (table_df['postTransactionAmounts'].iloc[-1] /
                           table_df['preTransactionAmounts'].iloc[0]).__round__(2)
        else:
            share_ratio = 5
        Cost = table_df['transactionPricePerShare'].mean()
        if share_ratio < 1:
            Transaction = 'Sale'
        else:
            Transaction = 'Buy'
        total_shares = table_df['postTransactionAmounts'].iloc[-1]
        return Cost, Transaction, share_ratio, total_shares, abs(noShares)

    def get_derivativeTrasactionTable(self, table_data):
        import pandas as pd

        if type(table_data['derivativeTransaction']) is not list:
            table_data['derivativeTransaction'] = [table_data['derivativeTransaction']]

        table_df = pd.DataFrame(table_data['derivativeTransaction'])

        Cost, Transaction, share_ratio, total_shares, noShares = self.get_trasactionTable(table_df)
        return Cost, Transaction, share_ratio, total_shares, noShares
