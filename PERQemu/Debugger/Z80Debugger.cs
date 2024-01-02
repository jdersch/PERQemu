﻿//
// Z80Debugger.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace PERQemu.Debugger
{
    /// <summary>
    /// Provides rudimentary debug support; at this time this is primarily mapping
    /// addresses to lines in corresponding source listings.
    /// </summary>
    public class Z80Debugger
    {
        public Z80Debugger(string listing)
        {
            LoadZ80Source(Paths.BuildPROMPath(listing));
        }

        public string GetSourceLineForAddress(ushort address)
        {
            if (_sourceMap.ContainsKey(address))
            {
                return _sourceMap[address];
            }

            return null;
        }

        public ushort GetAddressForSymbol(string symbol)
        {
            if (_symbolToAddressMap.ContainsKey(symbol))
            {
                return _symbolToAddressMap[symbol];
            }

            return 0;
        }

        public string GetSymbolForAddress(ushort address, out ushort offset)
        {
            // Handle the case where the .lst file doesn't contain any symbols
            // (for instance, debugging a new ROM with an empty/minimal listing)
            if (_addressToSymbolMap.Count == 0)
            {
                offset = 0;
                return "<origin>";
            }

            // Exact match?
            if (_addressToSymbolMap.ContainsKey(address))
            {
                offset = 0;
                return _addressToSymbolMap[address];
            }

            // Find the symbol nearest to the address
            // This could be done more efficiently.
            List<ushort> sortedAddresses = _addressToSymbolMap.Keys.ToList();

            sortedAddresses.Sort();

            for (int i = 0; i < sortedAddresses.Count; i++)
            {
                if (sortedAddresses[i] > address)
                {
                    if (i == 0)
                    {
                        // Nearest symbol is the start of the code
                        offset = address;
                        return "<origin>";
                    }

                    // Nearest symbol is the previous one we looked at
                    offset = (ushort)(address - sortedAddresses[i - 1]);
                    return _addressToSymbolMap[sortedAddresses[i - 1]];
                }
            }

            // Nearest symbol is the last
            ushort addr = sortedAddresses[sortedAddresses.Count - 1];
            offset = (ushort)(address - addr);
            return _addressToSymbolMap[addr];
        }

        void LoadZ80Source(string sourceFile)
        {
            _addressToSymbolMap = new Dictionary<ushort, string>();
            _symbolToAddressMap = new Dictionary<string, ushort>();
            _sourceMap = new Dictionary<ushort, string>();

            using (StreamReader sw = new StreamReader(sourceFile))
            {
                // Read the source listing in; scrape for symbol names, etc.
                while (!sw.EndOfStream)
                {
                    // Each line looks like:
                    //     ROM:addr <source code line>
                    // And each source code line may begin with a symbol (i.e. "PRQVEC:")
                    // or indented code.  There should be no badly-formed lines as the
                    // listing is machine-generated.  We'll try to be careful anyway.
                    var line = sw.ReadLine();

                    if (string.IsNullOrWhiteSpace(line))
                    {
                        throw new InvalidOperationException("Unexpected empty line in Z80 source listing");
                    }

                    var address = ushort.Parse(line.Substring(4, 4), System.Globalization.NumberStyles.HexNumber);
                    var source = line.Substring(8);

                    Log.Detail(Category.Z80Inst, "Loaded Z80 addr {0}, line '{1}'", address, source);

                    // Index the source
                    _sourceMap[address] = source;

                    // See if this line begins with a symbol and if so index it.
                    var tokens = source.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);

                    if (tokens.Length < 1)
                    {
                        continue;
                    }

                    if (tokens[0].EndsWith(":", StringComparison.Ordinal))
                    {
                        var symbol = tokens[0].Substring(0, tokens[0].Length - 1);

                        _symbolToAddressMap[symbol] = address;
                        _addressToSymbolMap[address] = symbol;
                    }
                }
            }
        }

        Dictionary<string, ushort> _symbolToAddressMap;
        Dictionary<ushort, string> _addressToSymbolMap;
        Dictionary<ushort, string> _sourceMap;
    }
}
