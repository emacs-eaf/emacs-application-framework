
#!/usr/bin/env python
# coding=utf-8

import json
import requests

class Jsonrpc(object):

    MUTI_METHOD = 'system.multicall'
    ADDURI_METHOD = 'aria2.addUri'

    def __init__(self, host, port, token=None):
        self._idCount = 0
        self.host = host
        self.port = port
        self.serverUrl = "http://{host}:{port}/jsonrpc".format(**locals())

    def _genParams(self, method , uris=None, options=None, cid=None):
        p = {
            'jsonrpc': '2.0',
            'id': self._idCount,
            'method': method,
            'test': 'test',
            'params': []
        }
        if uris:
            p['params'].append(uris)
        if options:
            p['params'].append(options)
        return p

    def _post(self, action, params, onSuccess, onFail=None):
        if onFail is None:
            onFail = Jsonrpc._defaultErrorHandle
        paramsObject = self._genParams(action, *params)
        resp = requests.post(self.serverUrl, data=json.dumps(paramsObject))
        result = resp.json()
        if "error" in result:
            return onFail(result["error"]["code"], result["error"]["message"])
        else:
            return onSuccess(resp)

    def addUris(self, uri, options=None):
        def success(response):
            return response.text
        return self._post(Jsonrpc.ADDURI_METHOD, [[uri,], options], success)


    @staticmethod
    def _defaultErrorHandle(code, message):
        print ("ERROR: {},{}".format(code, message))
        return None
