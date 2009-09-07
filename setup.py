#!/usr/bin/env python

from distutils.core import setup

setup(name='Moman',
      version='0.2.1',
      description='A tools suite for orthographic/grammatical check',
      author='Jean-Philippe Barrette-LaPierre',
      author_email='jpb_NO_SPAM@rrette.com',
      url='http://rrette.com/moman.html',
      packages=['finenight'],
      package_dir={'finenight': 'finenight/python/'},
      scripts=['finenight/python/recognize']
      )
