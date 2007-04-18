# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of scons-chicken.
#
# scons-chicken is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# scons-chicken is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with scons-chicken; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

import SCons.Tool
import os
import os.path
from SCons.Builder import Builder
from SCons.Node.FS import File
from string import strip, split

def generate(env):
    env["CHICKEN"] = env.Detect("chicken") or "chicken"
    env["CHICKENPROFLAGS"] = ""
    env["CHICKENEXTFLAGS"] = "-dynamic -feature chicken-compile-shared -feature compiling-extension"
    env["CHICKENREPOSITORY"] = strip(os.popen("chicken-setup -repository").read()) + "/"

    def chickenProGenerator(source, target, env, for_signature):
        """ Generate the actions to compile a Chicken program. """
        actions = []
        for s, t in zip(source, target):
            actions.append("$CHICKEN %s -output-file %s $CHICKENPROFLAGS" % (s, t))
        return actions

    env.ChickenPro = Builder(generator = chickenProGenerator,
                             sufix = ".c",
                             src_sufix = ".scm")
    
    def chickenExtGenerator(source, target, env, for_signature):
        """ Generate the actions to compile a Chicken extension. """
        actions = []
        for s, t in zip(source, target):
            actions.append("$CHICKEN %s -output-file %s $CHICKENEXTFLAGS" % (s, t))
        return actions
    
    env.ChickenExt = Builder(generator = chickenExtGenerator,
                             sufix = ".c",
                             src_sufix = ".scm")

    def ChickenProgram(env, target, source = None, *args, **kw):
        """Pseudo builder to make a Chicken program."""
        
        # Check if we have Chiken instaled.
        conf = env.Configure()
        if not conf.CheckLibWithHeader("chicken",
                                       "chicken.h",
                                       "c",
                                       "C_alloc(C_SIZEOF_LIST(3));"):
            print "It seems you don't have Chicken installed or it is not"
            print "installed correctly. For more information:"
            print "http://www.call-with-current-continuation.org/"
            exit(1)
        env = conf.Finish()

        # If no source provided, source is what is on target and target should be generated.
        if not source:
            source = target
            if isinstance(source, list):
                target = split(source[0], ".")[0]
            else:
                target = split(source, ".")[0]
                
        # Separate Scheme sources from the rest
        schemeSources, schemeAsCSources, otherSources = groupSources(source)

        # Compile Scheme sources into C using Chicken (for programs).
        env.ChickenPro(env, schemeAsCSources, schemeSources)

        # Add the needed compilation flags. They are added in this way because ParseConfig adds it to the environment and the same environment might be used for both, programs and extensions, and their cflags are conflicting.
        ccflags = strip(os.popen("csc -cflags").read())
        if kw.has_key("CCFLAGS"):
            kw["CCFLAGS"] += ccflags
        else:
            kw["CCFLAGS"] = ccflags
            
        # Add the needed libraries.
        env.ParseConfig("csc -libs")
        
        return apply(env.Program, (target, schemeAsCSources + otherSources) + args, kw)

    def ChickenExtension(env, target, source = None, *args, **kw):
        """Pseudo builder to make a Chicken extension."""
        
        # If no source provided, source is what is on target and target should be generated.
        if not source:
            source = target
            if isinstance(source, list):
                target = split(source[0], ".")[0]
            else:
                target = split(source, ".")[0]
            target = [target, target + ".setup"]
                
        # Separate Scheme sources from the rest
        schemeSources, schemeAsCSources, otherSources = groupSources(source)

        # Compile Scheme sources into C using Chicken (for programs).
        env.ChickenExt(env, schemeAsCSources, schemeSources)

        # Add the needed compilation flags. They are added in this way because ParseConfig adds it to the environment and the same environment might be used for both, programs and extensions, and their cflags are conflicting.
        ccflags = strip(os.popen("chicken-config -cflags").read())
        if kw.has_key("CCFLAGS"):
            kw["CCFLAGS"] += ccflags
        else:
            kw["CCFLAGS"] = ccflags
            
        # Add the needed libraries.
        env.ParseConfig("chicken-config -libs")

        kw["SHLIBPREFIX"] = ""
        lib = apply(env.SharedLibrary, (target, schemeAsCSources + otherSources) + args, kw)

        if kw.has_key("DOCUMENTATION"):
            documentation = kw["DOCUMENTATION"]
        else:
            documentation = ""

        if kw.has_key("SYNTAX"):
            syntax = kw["SYNTAX"]
        else:
            syntax = False

        if kw.has_key("REQUIRES"):
            requires = kw["REQUIRES"]
        else:
            requires = []
            
        # Generate the .setup file.
        setup = chickenSetup(os.path.splitext(str(lib[0]))[0] + ".setup", lib[0], documentation, syntax, requires)

        # Clean the .setup file when cleaning the library.
        env.Clean(lib, setup)

        return lib, setup

    # Attach the pseudo-Builders to the Environment so they can be called like a real Builder.
    env.ChickenProgram = ChickenProgram
    env.ChickenExtension = ChickenExtension
    
    def groupSources(sources):
        """ Separate the Scheme sources from the rest and generate the file names that the compiled-to-c sources are going to have. """
     
        if not isinstance(sources, list):
            sources = [sources]
        
        # Lists for the names of the scheme sources and others respectively.
        schemeSources = []
        schemeAsCSources = []
        otherSources = []

        # Separate sources into scheme, generated and other sources
        for s in sources:
            if os.path.splitext(s)[1] == ".scm":
                schemeSources.append(s)
                schemeAsCSources.append(os.path.splitext(s)[0]+".c")
            else:
                otherSources.append(s)

        return schemeSources, schemeAsCSources, otherSources
    
    def chickenSetup(setup, files, documentation = None, syntax = False, requires = None):
        """ This procedure works like a builder and it builds the .setup files.
            Parameters:
            1. env (any way to fix this ?)
            2. Name of the .setup file to generate.
            3. Name or list of names of the .so files that will be linked from the setup file.
            Optional parameters:
            documentation = Where is the HTML documentation.
            syntax = Whether (true or false) this contain syntax extensions.
            requires = other or list of other required extensions."""
        
        def makeLispList(head, items, prefix = ""):
            """ This procedure builds a string that resembles a Lisp list of strings.
                The first parameter is the header of the Lisp-like list.
                The second parameter is either a string or a list of strings that
                will form the Lisp-like list.
                Prefix is an optional parameter that will be prepended to each item
                on the list."""

            def buildPath(item):
                """ Procedure that builds a path using the prefix and a string or
                    File object."""
                if isinstance(item, str):
                    return prefix + item
                elif isinstance(item, list):
                    return prefix + str(item[0])
                elif isinstance(item, File):
                    return prefix + item.name
                else:
                    print "Type not recognized to build .setup file."
                    return ""
                
                
            l = "(" + head
            
            if isinstance(items, list):
                for i in items:
                    l += " \"" + buildPath(i) + "\" "
            else:
                l += " \"" + buildPath(items) + "\""
                
            l += ")" 
            return l

        # Open the list (a .setup is a list).
        content = "("

        # Make a list of the sources, the .so files. All located on CHICKENREPOSITOR.
        content += makeLispList("files", files, env["CHICKENREPOSITORY"])

        # Add the documentation.
        if documentation:
            content += "\n(documentation \"" + documentation + "\")"

        # Is this a syntax extension ?
        if syntax == True:
            content += "\n(syntax)"

        # What other extensions are necesary by this one ?
        if requires:
            # Make a list of extensions.
            content += "\n" + makeLispList("requires", requires)

        # Close the list.
        content += ")\n"

        # Write the list (being hold as a string on setup) to the file.
        setupFile = open(setup, "w")
        setupFile.write(content)
        setupFile.close()

        # Return an object representing the file for further handling.
        return env.File(setup)

    def findLibs(output, initialFlags = None):
        """ Parse the output of a config command, like chicken-config, and finds the libs and libpaths. """
        flags = {"LIBPATH":[],
                 "LIBS":[]}

        print output
        
        if initialFlags:
            flags.update(initialFlags)

        output = split(output)
        for item in output:
            if item[:2] == "-L":
                flags["LIBPATH"].append(item[2:])
            elif item[:2] == "-l":
                flags["LIBS"].append(item[2:])
                
        return flags

def exists(env):
    return env.Detect(["chicken"])
