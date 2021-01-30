class CircularAudioWave {
    constructor(elem, opts = {}) {
        this.opts = opts;
        this.lastMaxR = 0;
        this.maxChartValue = 240;
        this.minChartValue = 100;
        this.chart = echarts.init(elem);
        this.playing = false;
        this.lineColorOffset = 0;
        this.tick = 0;

        let bgColor = '#2E2733';
        this.defaultChartOption = {
            angleAxis: {
                type: 'value',
                clockwise: false,
                axisLine: {
                    show: false,
                },
                axisTick: {
                    show: false,
                },
                axisLabel: {
                    show: false,
                },
                splitLine: {
                    show: false,
                },
            },
            radiusAxis: {
                min: 0,
                max: this.maxChartValue + 50,
                axisLine: {
                    show: false,
                },
                axisTick: {
                    show: false,
                },
                axisLabel: {
                    show: false,
                },
                splitLine: {
                    show: false,
                },
            },
            polar: {
                radius: '100%',
            },
            series: [{
                    coordinateSystem: 'polar',
                    name: 'line',
                    type: 'line',
                    showSymbol: false,
                    lineStyle: {
                        color: {
                            colorStops: [{
                                    offset: 0.7,
                                    color: '#e91e63'
                                },
                                {
                                    offset: 0.3,
                                    color: '#3f51b5'
                                }
                            ],
                        },
                        shadowColor: 'blue',
                        shadowBlur: 10,
                    },
                    zlevel: 2,
                    data: Array.apply(null, {
                        length: 361
                    }).map(Function.call, i => {
                        return [this.minChartValue, i];
                    }),
                    silent: true,
                    hoverAnimation: false,
                },
                {
                    coordinateSystem: 'polar',
                    name: 'maxbar',
                    type: 'line',
                    showSymbol: false,
                    lineStyle: {
                        color: '#87b9ca',
                        shadowColor: '#87b9ca',
                        shadowBlur: 10,
                    },
                    data: Array.apply(null, {
                        length: 361
                    }).map(Function.call, i => {
                        return [this.minChartValue, i];
                    }),
                    silent: true,
                    hoverAnimation: false,
                },
                {
                    coordinateSystem: 'polar',
                    name: 'interior',
                    type: 'effectScatter',
                    showSymbol: false,
                    data: [0],
                    symbolSize: 100,
                    rippleEffect: {
                        period: 3.5,
                        scale: 3,
                    },
                    itemStyle: {
                        color: {
                            type: 'radial',
                            colorStops: [{
                                offset: 0,
                                color: '#87b9ca'
                            }, {
                                offset: 1,
                                color: 'white'
                            }],
                        },
                    },
                    silent: true,
                    hoverAnimation: false,
                    animation: false,
                },
            ]
        };
        // check if the default naming is enabled, if not use the chrome one.
        window.AudioContext = window.AudioContext || window.webkitAudioContext;
        window.OfflineAudioContext = window.OfflineAudioContext || window.webkitOfflineAudioContext;

        if (!window.AudioContext) {
            alert('Your browser does not support Web Audio API');
        } else {
            this.context = new AudioContext();
            this.offlineContext = new OfflineAudioContext(2, 30 * 44100, 44100);
            this.sourceNode = this.context.createBufferSource();
            this.offlineSource = this.offlineContext.createBufferSource();
            this.sourceNode.loop = !!this.opts.loop;
            this.analyser = this.context.createAnalyser();
        }


        if (this.opts.mode === 'sunburst') {
            let colors = ['#FFAE57', '#FF7853', '#EA5151', '#CC3F57', '#9A2555'];

            let data = [{
                    children: [{
                        children: []
                    }],
                },
                {
                    children: [{
                        children: []
                    }],
                },
            ];
            for (let i = 0; i < 5; i++) {
                data[0].children[0].children.push({
                    name: '-',
                    children: [{
                        name: ''
                    }]
                }, );
                data[1].children[0].children.push({
                    name: '-',
                    children: [{
                        name: ''
                    }]
                }, );
            }

            // loop to the bottom children
            data.forEach(level0 => {
                level0.children.forEach(level1 => {
                    level1.children.forEach((item) => {
                        item.children[0].value = 1;
                    })
                })
            });

            this.defaultChartOption = {
                backgroundColor: bgColor,
                color: colors,
                series: [{
                    type: 'sunburst',
                    center: ['50%', '48%'],
                    data: data,
                    nodeClick: false,
                    sort: function (a, b) {
                        if (a.depth === 1) {
                            return b.getValue() - a.getValue();
                        } else {
                            return a.dataIndex - b.dataIndex;
                        }
                    },
                    itemStyle: {
                        borderColor: bgColor,
                        borderWidth: 2
                    },
                    levels: [{}, {
                        r0: 0,
                        r: 40,
                    }, {
                        r0: 40,
                        r: 105
                    }, {
                        r0: 115,
                        r: 140,
                        itemStyle: {
                            shadowBlur: 2,
                            shadowColor: colors[2],
                            color: 'transparent'
                        },
                        label: {
                            rotate: 'tangential',
                            fontSize: 10,
                            color: colors[0]
                        }
                    }, {
                        r0: 140,
                        r: 145,
                        itemStyle: {
                            shadowBlur: 80,
                            shadowColor: colors[0],
                            color: colors[0]
                        },
                        label: {
                            position: 'outside',
                            textShadowBlur: 5,
                            textShadowColor: '#333',
                            backgroundColor: colors[0],
                        },
                    }]
                }]
            };
        }

        this.chartOption = JSON.parse(JSON.stringify(this.defaultChartOption));
    }
    loadAudio(filePath) {
        console.log(filePath);
        this.filePath = filePath;
        this._setupAudioNodes();
        this._setupOfflineContext();
        var request = new XMLHttpRequest();
        request.open('GET', filePath, true);
        request.responseType = 'arraybuffer';
        request.send();
        return new Promise((resolve, reject) => {
            request.onload = () => {
                // Preprocess buffer for bpm
                this.offlineContext.decodeAudioData(request.response, buffer => {
                    this.sourceNode.buffer = buffer;
                    this.offlineSource.buffer = buffer;
                    this.offlineSource.start(0);
                    this.offlineContext.startRendering();
                });

                this.offlineContext.oncomplete = e => {
                    let buffer = e.renderedBuffer;
                    this.bpm = this._getBPM([buffer.getChannelData(0), buffer.getChannelData(1)]);

                    this._init();
                    resolve();

                    this.play();
                };

            };
        });
    }
    _init() {
        this.chart.setOption(this.chartOption, true);
        this._debouncedDraw = this._debounce(this._drawAnimation.bind(this), 25);
    }
    presetOption() {
        if (this.opts.mode !== 'sunburst') {
            this.chartOption.series[0].animation = false;
            this.chartOption.series[2].rippleEffect.period = 150 / this.bpm;
        }
    }

    play() {
        if (this.sourceNode && this.sourceNode.buffer) {
            this.playing = true;
            this.presetOption();
            this.sourceNode.start(0);
            this._debouncedDraw();
        } else {
            alert('Audio is not ready');
        }
    }
    // TODO
    pause() {

    }
    destroy() {
        this.chart.dispose();
    }
    reset() {
        this.tick = 0;
        this.chartOption = JSON.parse(JSON.stringify(this.defaultChartOption));
        this._init();
    }
    // TODO: Allow callback
    onended() {
        if (!this.opts.loop) {
            this.playing = false;
            this.context.close();
            this.sourceNode.buffer = null;
            this.offlineSource.buffer = null;
            this.reset();

            this.context = new AudioContext();
            this.offlineContext = new OfflineAudioContext(2, 30 * 44100, 44100);
            this.sourceNode = this.context.createBufferSource();
            this.offlineSource = this.offlineContext.createBufferSource();
            this.analyser = this.context.createAnalyser();
            this.loadAudio(this.filePath);
        }
    }
    _setupAudioNodes() {
        this.analyser.smoothingTimeConstant = 0.3;
        this.analyser.fftSize = 2048;

        this.sourceNode.connect(this.analyser);

        this.sourceNode.connect(this.context.destination);
        this.sourceNode.onended = this.onended.bind(this);
    }

    _setupOfflineContext() {
        // Beats generally occur around the 100 to 150 hz range.
        let lowpass = this.offlineContext.createBiquadFilter();
        lowpass.type = "lowpass";
        lowpass.settar
        lowpass.frequency.setValueAtTime(150, 0);
        lowpass.Q.setValueAtTime(1, 0);

        this.offlineSource.connect(lowpass);

        let highpass = this.offlineContext.createBiquadFilter();
        highpass.type = "highpass";
        highpass.frequency.setValueAtTime(100, 0);
        highpass.Q.setValueAtTime(1, 0);
        lowpass.connect(highpass);
        highpass.connect(this.offlineContext.destination);
    }

    _drawAnimation() {
        let freqData = new Uint8Array(this.analyser.frequencyBinCount);
        this.analyser.getByteFrequencyData(freqData);
        this._draw(freqData);
        requestAnimationFrame(this._debouncedDraw.bind(this));
    }

    _draw(freqData) {
        if (this.playing) {
            let waveData = this._generateWaveData(freqData);
            this.chartOption.series[0].data = waveData.data;

            if (waveData.maxR > this.lastMaxR) {
                this.lastMaxR = waveData.maxR + 4;
            } else if (this.playing) {
                this.lastMaxR -= 2;
            } else {
                this.lastMaxR = this.minChartValue;
            }

            if (this.opts.mode !== 'sunburst') {
                // maxbar
                this.chartOption.series[1].data = Array.apply(null, {
                    length: 361
                }).map(Function.call, (i) => {
                    return [this.lastMaxR, i];
                });
            }
            this.chart.setOption(this.chartOption, true);
            this.tick++;
        }
    }
    _generateWaveData(data) {
        let waveData = [];
        let maxR = 0;
        if (this.opts.mode !== 'sunburst') {
            for (let i = 0; i <= 360; i++) {
                // (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
                let freq = data[i];
                var r = (((freq - 0) * (this.maxChartValue - this.minChartValue)) / (255 - 0)) + this.minChartValue;
                if (r > maxR) {
                    maxR = r;
                }
                waveData.push([r, i]);
            }
            waveData.push([waveData[0][0], 360]);
        } else {
            waveData = JSON.parse(JSON.stringify(this.chartOption.series[0].data));;
            let index = 0;
            waveData.forEach(level0 => {
                level0.children.forEach(level1 => {
                    level1.children.forEach(item => {
                        let freq = data[index];
                        var r = (((freq - 0) * (40 - 0)) / (255 - 0)) + 0;

                        item.children[0].name = Array.apply(null, {
                            length: r
                        }).map(Function.call, i => {
                            return '';
                        }).join(' ');
                        index++;
                    })
                })
            });
        }
        return {
            maxR: maxR,
            data: waveData
        };
    };


    _getBPM(data) {
        let partSize = 22050,
            parts = data[0].length / partSize,
            peaks = [];

        for (let i = 0; i < parts; i++) {
            let max = 0;
            for (let j = i * partSize; j < (i + 1) * partSize; j++) {
                let volume = Math.max(Math.abs(data[0][j]), Math.abs(data[1][j]));
                if (!max || (volume > max.volume)) {
                    max = {
                        position: j,
                        volume: volume
                    };
                }
            }
            peaks.push(max);
        }

        peaks.sort((a, b) => {
            return b.volume - a.volume;
        });
        peaks = peaks.splice(0, peaks.length * 0.5);
        peaks.sort((a, b) => {
            return a.position - b.position;
        });

        let groups = [];
        peaks.forEach((peak, index) => {
            for (let i = 1;
                (index + i) < peaks.length && i < 10; i++) {
                let group = {
                    bpm: (60 * 44100) / (peaks[index + i].position - peak.position),
                    count: 1
                };

                while (group.bpm < 90) {
                    group.bpm *= 2;
                }

                while (group.bpm > 180) {
                    group.bpm /= 2;
                }

                group.bpm = Math.round(group.bpm);

                if (!(groups.some(interval => {
                        return (interval.bpm === group.bpm ? interval.count++ : 0);
                    }))) {
                    groups.push(group);
                }
            }
        });

        let bpm = groups.sort((intA, intB) => {
            return intB.count - intA.count;
        })[0].bpm;
        console.log('bpm:', bpm);

        return bpm;
    }

    _debounce(func, wait, immediate) {
        var timeout;
        return function () {
            var context = this,
                args = arguments;
            var later = function () {
                timeout = null;
                if (!immediate) func.apply(context, args);
            };
            var callNow = immediate && !timeout;
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
            if (callNow) func.apply(context, args);
        };
    };
}
