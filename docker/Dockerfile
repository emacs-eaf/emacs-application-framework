FROM base/archlinux

RUN pacman -Syy && \
    pacman -S sudo git emacs libpulse nss python python-pip python-dbus  --noconfirm && \
    yes|pacman -Scc 

RUN pip install --upgrade pip

RUN pip install PyMuPDF grip qrcode xlib pyqt5 && rm -rf /root/.cache

RUN git clone --depth=1 https://github.com/manateelazycat/emacs-application-framework

ARG _UID="1000"
ARG _USER="eaf"
RUN useradd --uid ${_UID}  -ms /bin/bash ${_USER}
RUN echo "${_USER}     ALL=NOPASSWD: ALL" >> /etc/sudoers

ENV LANG=zh_CN.UTF-8

USER ${_USER}
WORKDIR /home/${_USER}

CMD ["emacs","-L","/emacs-application-framework","--eval","(require 'eaf)"]
