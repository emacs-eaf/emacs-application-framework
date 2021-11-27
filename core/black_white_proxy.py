#!/usr/bin/env python
# coding=utf-8

import re

from PyQt5.QtNetwork import QNetworkProxy
from PyQt5.QtWebEngineCore import QWebEngineUrlRequestInterceptor

class BlackWhileListRequestInterceptor(QWebEngineUrlRequestInterceptor):
    """
    Proxy factory that enables non-default proxy list when
    requested URL is matched by one of whitelist patterns
    while not being matched by one of the blacklist patterns.
    """
    is_proxy = False

    def __init__(self, proxy=None, blacklist=None, whitelist=None, parent=None):

        self.proxy = proxy or None
        self.blacklist = blacklist or []
        self.whitelist = whitelist or []

        if proxy:
            self.proxy_policy = QNetworkProxy()
            if proxy[0] == "socks5":
                self.proxy_policy.setType(QNetworkProxy.Socks5Proxy)
            elif proxy[0] == "http":
                self.proxy_policy.setType(QNetworkProxy.HttpProxy)
            self.proxy_policy.setHostName(proxy[1])
            self.proxy_policy.setPort(int(proxy[2]))
        else:
            self.proxy_policy = None

        self.default_policy = QNetworkProxy()
        self.default_policy.setType(QNetworkProxy.NoProxy)


        super().__init__(parent)

    def shouldUseDefault(self, url):
        if not self.proxy:
            return True

        if not self.is_proxy:
            return True

        if any(re.match(p, url) if isinstance(p, str) else False for p in self.blacklist):
            return False

        if any(re.match(p, url) if isinstance(p, str) else False for p in self.whitelist):
            return True

        return bool(self.blacklist)

    def interceptRequest(self, info):
        url = info.requestUrl().toString()

        if self.shouldUseDefault(url):
            if QNetworkProxy.applicationProxy() != self.default_policy:
                QNetworkProxy.setApplicationProxy(self.default_policy)
        else:
            if QNetworkProxy.applicationProxy() != self.proxy_policy:
                QNetworkProxy.setApplicationProxy(self.proxy_policy)
