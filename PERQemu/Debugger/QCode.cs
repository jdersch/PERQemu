//
// QCode.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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
using System.Collections.Generic;

namespace PERQemu.Debugger
{
    /// <summary>
    /// List of known/loadable Qcode mappings.  Expose these so the Debugger
    /// can allow the user to switch them through the CLI.
    /// </summary>
    public enum QCodeSets
    {
        POSV4 = 0,
        AccentV4,
        AccentV5,
        PNXCCodes
    }

    /// <summary>
    /// Represents a single QCode instruction; includes a mapping from the Code
    /// to its Mnemonic.  For two-byte opcodes, setting the prefix flag tells
    /// the disassembler to look up the next byte in the dictionary to complete
    /// the opcode.
    /// </summary>
    public class QCode
    {
        public QCode(byte code, string mnemonic, bool prefix = false)
        {
            Code = code;
            IsPrefix = prefix;
            Mnemonic = mnemonic;
        }

        public byte Code;
        public bool IsPrefix;
        public string Mnemonic;
    }

    /// <summary>
    /// Contains methods and structures to aid in disassembling POS and Accent
    /// Q-codes.  Somewhat useful when debugging emulation issues in the Q-Code
    /// interpreter.  Now understands two-byte opcodes by maintaining a small
    /// bit of state: if a prefix opcode is executed via NextInst, the following
    /// byte from the op file is used in a dictionary lookup to identify the
    /// rest of the extended opcode sequence.
    /// </summary>
    public static class QCodeHelper
    {
        // TODO: Add PNX C-codes if any documentation can be found;
        // TODO: Is there _any_ way to make detection automatic when the OS is
        //       loaded?  It would be pretty spectacular to detect the switch
        //       from Q-code to Lisp instruction sets in Accent on-the-fly...
        static QCodeHelper()
        {
            Clear();
            _loaded = QCodeSets.POSV4;
            LoadQCodeSet(QCodeSets.POSV4);      // Default
        }

        public static QCodeSets Loaded => _loaded;

        public static QCode GetQCodeFromOpCode(byte opCode)
        {
            QCode q = _qCodesOrdered[opCode];

            if (q == null)
            {
                q = new QCode(opCode, "!! INVALID QCODE !!");
            }

            return q;
        }

        public static QCode GetExtendedOpCode(ushort extOpCode)
        {
            if (_qCodesExtended.ContainsKey(extOpCode))
            {
                return _qCodesExtended[extOpCode];
            }

            return new QCode((byte)extOpCode, "!! INVALID EXOP !!");
        }

        /// <summary>
        /// Clears and reloads the QCode table and the extended ops dictionary.
        /// </summary>
        public static void LoadQCodeSet(QCodeSets which)
        {
            switch (which)
            {
                case QCodeSets.POSV4:
                    Clear();
                    AddToTable(_qCodesCommon);
                    AddToTable(_qCodesPOSV4);
                    AddToDictionary(250, _qCodesPOSV4rops);
                    AddToDictionary(252, _qCodesPOSV4lops);
                    break;

                case QCodeSets.AccentV4:
                    Clear();
                    AddToTable(_qCodesCommon);
                    AddToTable(_qCodesAccentV4);
                    // AddToDictionary(_qCodesAccentV4kops);
                    break;

                case QCodeSets.AccentV5:
                    Clear();
                    AddToTable(_qCodesCommon);
                    AddToTable(_qCodesAccentV5);
                    AddToDictionary(63, _qCodesAccentV5exops);
                    AddToDictionary(252, _qCodesAccentV5kops);
                    break;

                default:
                    throw new InvalidOperationException($"Unknown Qcode set {which}");
            }

            _loaded = which;
            Log.Debug(Category.QCode, "Loaded QCode set {0}", which);
        }

        /// <summary>
        /// Clears the "ordered" list of opcodes and extended dictionary.
        /// </summary>
        private static void Clear()
        {
            _qCodesOrdered = new QCode[256];
            _qCodesExtended = new Dictionary<ushort, QCode>();

            Log.Debug(Category.QCode, "Tables cleared.");
        }

        /// <summary>
        /// Adds QCodes to the table (indexed by opcode number).
        /// </summary>
        private static void AddToTable(QCode[] unordered)
        {
            for (int i = 0; i < unordered.Length; i++)
            {
                int index = unordered[i].Code;

                if (_qCodesOrdered[index] != null)
                {
                    // throw new InvalidOperationException($"Duplicate QCode entry {index}");
                    Log.Warn(Category.QCode, "Duplicate QCode entry {0}", index);
                }

                _qCodesOrdered[index] = unordered[i];
            }
        }

        /// <summary>
        /// Adds extended opcodes to the dictionary.  Index is simply the prefix
        /// byte concatenated with the opcode byte in the obvious way.
        /// </summary>
        private static void AddToDictionary(byte prefix, QCode[] exops)
        {
            for (var i = 0; i < exops.Length; i++)
            {
                var key = (ushort)((prefix << 8) | exops[i].Code);

                if (_qCodesExtended.ContainsKey(key))
                {
                    // throw new InvalidOperationException($"Duplicate Extended QCode entry {key}");
                    Log.Warn(Category.QCode, "Duplicate Extended QCode entry {0}", key);
                }

                _qCodesExtended[key] = exops[i];
            }
        }

        /// <summary>
        /// Ordered table of QCodes, computed at runtime.
        /// </summary>
        private static QCode[] _qCodesOrdered;
        private static Dictionary<ushort, QCode> _qCodesExtended;
        private static QCodeSets _loaded;

        /// <summary>
        /// QCodes common to POS and Accent (all versions).
        /// </summary>
        private static QCode[] _qCodesCommon =
        {
            new QCode(0, "LDC0"),
            new QCode(1, "LDC1"),
            new QCode(2, "LDC2"),
            new QCode(3, "LDC3"),
            new QCode(4, "LDC4"),
            new QCode(5, "LDC5"),
            new QCode(6, "LDC6"),
            new QCode(7, "LDC7"),
            new QCode(8, "LDC8"),
            new QCode(9, "LDC9"),
            new QCode(10, "LDC10"),
            new QCode(11, "LDC11"),
            new QCode(12, "LDC12"),
            new QCode(13, "LDC13"),
            new QCode(14, "LDC14"),
            new QCode(15, "LDC15"),
            new QCode(16, "LDCMO"),
            new QCode(17, "LDCB"),
            new QCode(18, "LDCW"),
            new QCode(254, "BREAK"),
            new QCode(255, "REFILLOP")
        };

        /// <summary>
        /// Unordered Table of QCodes for POS, based on the older QCode V4 (or
        /// earlier?).  These don't _need_ to be in order, but they happen to be
        /// except for invalid opcodes which are skipped.
        /// </summary>
        private static QCode[] _qCodesPOSV4 =
        {
            // 0..18 in common
            new QCode(19, "LSA"),
            new QCode(20, "ROTSHI"),
            new QCode(21, "STIND"),
            new QCode(22, "LDCN"),
            new QCode(23, "LDB"),
            new QCode(24, "STB"),
            new QCode(25, "LDCH"),
            new QCode(26, "LDP"),
            new QCode(27, "STP"),
            new QCode(28, "STCH"),
            new QCode(29, "EXGO"),
            new QCode(30, "LAND"),
            new QCode(31, "LOR"),
            new QCode(32, "LNOT"),
            new QCode(33, "EQUBool"),
            new QCode(34, "NEQBool"),
            new QCode(35, "LEQBool"),
            new QCode(36, "LESBool"),
            new QCode(37, "GEQBool"),
            new QCode(38, "GTRBool"),
            new QCode(39, "EQUI"),
            new QCode(40, "NEQI"),
            new QCode(41, "LEQI"),
            new QCode(42, "LESI"),
            new QCode(43, "GEQI"),
            new QCode(44, "GTRI"),
            // 45..50 undefined
            new QCode(51, "EQUStr"),
            new QCode(52, "NEQStr"),
            new QCode(53, "LEQStr"),
            new QCode(54, "LESStr"),
            new QCode(55, "GEQStr"),
            new QCode(56, "GTRStr"),
            new QCode(57, "EQUByt"),
            new QCode(58, "NEQByt"),
            new QCode(59, "LEQByt"),
            new QCode(60, "LESByt"),
            new QCode(61, "GEQByt"),
            new QCode(62, "GTRByt"),
            new QCode(63, "EQUPowr"),
            new QCode(64, "NEQPowr"),
            new QCode(65, "LEQPowr"),
            new QCode(66, "SGS"),
            new QCode(67, "GEQPower"),
            new QCode(68, "SRS"),
            new QCode(69, "EQUWord"),
            new QCode(70, "NEQWord"),
            new QCode(71, "ABI"),
            new QCode(72, "ADI"),
            new QCode(73, "NGI"),
            new QCode(74, "SBI"),
            new QCode(75, "MPI"),
            new QCode(76, "DVI"),
            new QCode(77, "MODI"),
            new QCode(78, "CHK"),
            // 79..87 undefined
            new QCode(88, "INN"),
            new QCode(89, "UNI"),
            new QCode(90, "QINT"),
            new QCode(91, "DIF"),
            new QCode(92, "EXIT"),
            new QCode(93, "NOOP"),
            new QCode(94, "REPL"),
            new QCode(95, "REPL2"),
            new QCode(96, "MMS"),
            new QCode(97, "MES"),
            new QCode(98, "LVRD"),
            new QCode(99, "LSSN"),
            new QCode(100, "XJP"),
            new QCode(101, "PSW"),          // new?
            new QCode(102, "RASTEROP"),
            new QCode(103, "STARTIO"),
            new QCode(104, "PBLK"),         // new?
            new QCode(105, "INTOFF"),
            new QCode(106, "INTON"),
            new QCode(107, "LDLB"),
            new QCode(108, "LDLW"),
            new QCode(109, "LDL0"),
            new QCode(110, "LDL1"),
            new QCode(111, "LDL2"),
            new QCode(112, "LDL3"),
            new QCode(113, "LDL4"),
            new QCode(114, "LDL5"),
            new QCode(115, "LDL6"),
            new QCode(116, "LDL7"),
            new QCode(117, "LDL8"),
            new QCode(118, "LDL9"),
            new QCode(119, "LDL10"),
            new QCode(120, "LDL11"),
            new QCode(121, "LDL12"),
            new QCode(122, "LDL13"),
            new QCode(123, "LDL14"),
            new QCode(124, "LDL15"),
            new QCode(125, "LLAB"),
            new QCode(126, "LLAW"),
            new QCode(127, "STLB"),
            new QCode(128, "STLW"),
            new QCode(129, "STL0"),
            new QCode(130, "STL1"),
            new QCode(131, "STL2"),
            new QCode(132, "STL3"),
            new QCode(133, "STL4"),
            new QCode(134, "STL5"),
            new QCode(135, "STL6"),
            new QCode(136, "STL7"),
            new QCode(137, "LDOB"),
            new QCode(138, "LDOW"),
            new QCode(139, "LDO0"),
            new QCode(140, "LDO1"),
            new QCode(141, "LDO2"),
            new QCode(142, "LDO3"),
            new QCode(143, "LDO4"),
            new QCode(144, "LDO5"),
            new QCode(145, "LDO6"),
            new QCode(146, "LDO7"),
            new QCode(147, "LDO8"),
            new QCode(148, "LDO9"),
            new QCode(149, "LDO10"),
            new QCode(150, "LDO11"),
            new QCode(151, "LDO12"),
            new QCode(152, "LDO13"),
            new QCode(153, "LDO14"),
            new QCode(154, "LDO15"),
            new QCode(155, "LOAB"),
            new QCode(156, "LOAW"),
            new QCode(157, "STOB"),
            new QCode(158, "STOW"),
            new QCode(159, "STO0"),
            new QCode(160, "STO1"),
            new QCode(161, "STO2"),
            new QCode(162, "STO3"),
            new QCode(163, "STO4"),
            new QCode(164, "STO5"),
            new QCode(165, "STO6"),
            new QCode(166, "STO7"),
            new QCode(167, "MVBB"),
            new QCode(168, "MVBW"),
            new QCode(169, "MOVB"),
            new QCode(170, "MOVW"),
            new QCode(171, "INDB"),
            new QCode(172, "INDW"),
            new QCode(173, "IND0/LDIND"),
            new QCode(174, "IND1"),
            new QCode(175, "IND2"),
            new QCode(176, "IND3"),
            new QCode(177, "IND4"),
            new QCode(178, "IND5"),
            new QCode(179, "IND6"),
            new QCode(180, "IND7"),
            new QCode(181, "LGAWW"),
            new QCode(182, "STMW"),
            new QCode(183, "STDW"),
            new QCode(184, "SAS"),
            new QCode(185, "ADJ"),
            new QCode(186, "CALL"),
            new QCode(187, "CALLV"),
            new QCode(188, "ATPB"),
            new QCode(189, "ATPW"),
            new QCode(190, "WCS"),
            new QCode(191, "JCS"),
            new QCode(192, "LDGB"),
            new QCode(193, "LDGW"),
            new QCode(194, "LGAB"),
            new QCode(195, "LGAW"),
            new QCode(196, "STGB"),
            new QCode(197, "STGW"),
            // 198..199 undefined
            new QCode(200, "RETURN"),
            new QCode(201, "MMS2"),
            new QCode(202, "MES2"),
            new QCode(203, "LDTP"),
            new QCode(204, "JMPB"),
            new QCode(205, "JMPW"),
            new QCode(206, "JFB"),
            new QCode(207, "JFW"),
            new QCode(208, "JTB"),
            new QCode(209, "JTW"),
            new QCode(210, "JEQB"),
            new QCode(211, "JEQW"),
            new QCode(212, "JNEB"),
            new QCode(213, "JNEW"),
            new QCode(214, "IXP"),
            new QCode(215, "LDIB"),
            new QCode(216, "LDIW"),
            new QCode(217, "LIAB"),
            new QCode(218, "LIAW"),
            new QCode(219, "STIB"),
            new QCode(220, "STIW"),
            new QCode(221, "IXAB"),
            new QCode(222, "IXAW"),
            new QCode(223, "IXA1"),
            new QCode(224, "IXA2"),
            new QCode(225, "IXA3"),
            new QCode(226, "IXA4"),
            new QCode(227, "TLATE0"),       // TLATE1
            new QCode(228, "TLATE1"),       // TLATE2
            new QCode(229, "TLATE2"),       // TLATE3
            new QCode(230, "EXCH"),
            new QCode(231, "EXCH2"),
            new QCode(232, "INCB"),
            new QCode(233, "INCW"),
            new QCode(234, "CALLXB"),
            new QCode(235, "CALLXW"),
            new QCode(236, "LDMC"),
            new QCode(237, "LDDC"),
            new QCode(238, "LDMW"),
            new QCode(239, "LDDW"),
            new QCode(240, "STLATE"),
            new QCode(241, "LINE"),
            new QCode(242, "ENABLE"),
            new QCode(243, "QRAISE"),
            new QCode(244, "LDAP"),
            new QCode(250, "ROPS", true),   // Real Ops
            new QCode(251, "INCDDS"),
            new QCode(252, "LOPS", true),   // Long Ops
            new QCode(253, "KOPS", true)    // Spice Kernel Ops (unused by POS)
            // 254..255 in common
        };

        /// <summary>
        /// Second byte of extended POS V4 Real Ops.
        /// </summary>
        private static QCode[] _qCodesPOSV4rops =
        {
            new QCode(0, "TNC"),
            new QCode(1, "FLT"),
            new QCode(2, "ADR"),
            new QCode(3, "NGR"),
            new QCode(4, "SBR"),
            new QCode(5, "MPR"),
            new QCode(6, "DVR"),
            new QCode(7, "RND"),
            new QCode(8, "ABR"),
            new QCode(9, "EQUReal"),
            new QCode(10, "NEQReal"),
            new QCode(11, "LEQReal"),
            new QCode(12, "LESReal"),
            new QCode(13, "GEQReal"),
            new QCode(14, "GTRReal"),
            new QCode(15, "RUNUSED")
        };

        /// <summary>
        /// Second byte of extended POS V4 Long Ops.
        /// </summary>
        private static QCode[] _qCodesPOSV4lops =
        {
            new QCode(0, "CVTLI"),
            new QCode(1, "CVTIL"),
            new QCode(2, "ADL"),
            new QCode(3, "NGL"),
            new QCode(4, "SBL"),
            new QCode(5, "MPL"),
            new QCode(6, "DVL"),
            new QCode(7, "MODL"),
            new QCode(8, "ABL"),
            new QCode(9, "EQULong"),
            new QCode(10, "NEQLong"),
            new QCode(11, "LEQLong"),
            new QCode(12, "LESLong"),
            new QCode(13, "GEQLong"),
            new QCode(14, "GTRLong"),
            new QCode(15, "LUNUSED")
        };

        /// <summary>
        /// Unordered Table of QCodes for Accent S4 (and possibly later versions).
        /// These don't _need_ to be in order, but they happen to be except for
        /// invalid opcodes which are skipped.
        /// </summary>
        private static QCode[] _qCodesAccentV4 =
        {
            // 0..18 in common
            new QCode(19, "LSA"),
            new QCode(20, "ROTSHI"),
            new QCode(21, "STIND"),
            new QCode(22, "LDCN"),
            new QCode(23, "LDB"),
            new QCode(24, "STB"),
            new QCode(25, "LDCH"),
            new QCode(26, "LDP"),
            new QCode(27, "STP"),
            new QCode(28, "STCH"),
            new QCode(29, "EXGO"),
            new QCode(30, "LAND"),
            new QCode(31, "LOR"),
            new QCode(32, "LNOT"),
            new QCode(33, "EQUBool"),
            new QCode(34, "NEQBool"),
            new QCode(35, "LEQBool"),
            new QCode(36, "LESBool"),
            new QCode(37, "GEQBool"),
            new QCode(38, "GTRBool"),
            new QCode(39, "EQUI"),
            new QCode(40, "NEQI"),
            new QCode(41, "LEQI"),
            new QCode(42, "LESI"),
            new QCode(43, "GEQI"),
            new QCode(44, "GTRI"),
            new QCode(45, "EQUptr"),
            new QCode(46, "NEQPtr"),
            new QCode(47, "LLLB"),
            new QCode(48, "LLLW"),
            new QCode(49, "LILB"),
            new QCode(50, "LILW"),
            new QCode(51, "EQUStr"),
            new QCode(52, "NEQStr"),
            new QCode(53, "LEQStr"),
            new QCode(54, "LESStr"),
            new QCode(55, "GEQStr"),
            new QCode(56, "GTRStr"),
            new QCode(57, "EQUByt"),
            new QCode(58, "NEQByt"),
            new QCode(59, "LEQByt"),
            new QCode(60, "LESByt"),
            new QCode(61, "GEQByt"),
            new QCode(62, "GTRByt"),
            new QCode(63, "EQUPowr"),
            new QCode(64, "NEQPowr"),
            new QCode(65, "LEQPower"),
            new QCode(66, "SGS"),
            new QCode(67, "GEQPower"),
            new QCode(68, "SRS"),
            new QCode(69, "EQUWord"),
            new QCode(70, "NEQWord"),
            new QCode(71, "ABI"),
            new QCode(72, "ADI"),
            new QCode(73, "NGI"),
            new QCode(74, "SBI"),
            new QCode(75, "MPI"),
            new QCode(76, "DVI"),
            new QCode(77, "MODI"),
            new QCode(78, "CHK"),
            new QCode(79, "LOLB"),
            new QCode(80, "LOLW"),
            new QCode(81, "SLLB"),
            new QCode(82, "SLLW"),
            new QCode(83, "SILB"),
            new QCode(84, "SILW"),
            new QCode(85, "SOLB"),
            new QCode(86, "SOLW"),
            new QCode(87, "GOTOOVL"),
            new QCode(88, "INN"),
            new QCode(89, "UNI"),
            new QCode(90, "QINT"),
            new QCode(91, "DIF"),
            new QCode(92, "EXITT"),
            new QCode(93, "NOOP"),
            new QCode(94, "REPL"),
            new QCode(95, "REPL2"),
            new QCode(96, "MMS"),
            new QCode(97, "MES"),
            new QCode(98, "LVRD"),
            new QCode(99, "LDRET1"),
            new QCode(100, "XJP"),
            new QCode(101, "LDRET2"),
            new QCode(102, "RASTEROP"),
            new QCode(103, "STARTIO"),
            new QCode(104, "STRROP"),
            new QCode(105, "LDBIND"),
            new QCode(106, "STBIND"),
            new QCode(107, "LDLB"),
            new QCode(108, "LDLW"),
            new QCode(109, "LDL0"),
            new QCode(110, "LDL1"),
            new QCode(111, "LDL2"),
            new QCode(112, "LDL3"),
            new QCode(113, "LDL4"),
            new QCode(114, "LDL5"),
            new QCode(115, "LDL6"),
            new QCode(116, "LDL7"),
            new QCode(117, "LDL8"),
            new QCode(118, "LDL9"),
            new QCode(119, "LDL10"),
            new QCode(120, "LDL11"),
            new QCode(121, "LDL12"),
            new QCode(122, "LDL13"),
            new QCode(123, "LDL14"),
            new QCode(124, "LDL15"),
            new QCode(125, "LLAB"),
            new QCode(126, "LLAW"),
            new QCode(127, "STLB"),
            new QCode(128, "STLW"),
            new QCode(129, "STL0"),
            new QCode(130, "STL1"),
            new QCode(131, "STL2"),
            new QCode(132, "STL3"),
            new QCode(133, "STL4"),
            new QCode(134, "STL5"),
            new QCode(135, "STL6"),
            new QCode(136, "STL7"),
            new QCode(137, "LDOB"),
            new QCode(138, "LDOW"),
            new QCode(139, "LDREG0"),
            new QCode(140, "LDREG1"),
            new QCode(141, "LDREG2"),
            new QCode(142, "LDREG3"),
            new QCode(143, "STREG0"),
            new QCode(144, "STREG1"),
            new QCode(145, "STREG2"),
            new QCode(146, "STREG3"),
            new QCode(147, "LDLCB"),
            new QCode(148, "LDLIND"),
            new QCode(149, "UNDF149"),
            new QCode(150, "EXOP"),
            new QCode(151, "LDLC1"),
            new QCode(152, "LDLCM1"),
            new QCode(153, "nCVTLI"),
            new QCode(154, "nCVTIL"),
            new QCode(155, "LOAB"),
            new QCode(156, "LOAW"),
            new QCode(157, "STOB"),
            new QCode(158, "STOW"),
            new QCode(159, "nADL"),
            new QCode(160, "nSBL"),
            new QCode(161, "nEQLONG"),
            new QCode(162, "nNEQLONG"),
            new QCode(163, "nLEQLONG"),
            new QCode(164, "nLESLONG"),
            new QCode(165, "nGEQLONG"),
            new QCode(166, "nGTRLONG"),
            new QCode(167, "MVBB"),
            new QCode(168, "MVBW"),
            new QCode(169, "MOVB"),
            new QCode(170, "MOVW"),
            new QCode(171, "INDB"),
            new QCode(172, "INDW"),
            new QCode(173, "LDIND/IND0"),
            new QCode(174, "IND1"),
            new QCode(175, "IND2"),
            new QCode(176, "IND3"),
            new QCode(177, "IND4"),
            new QCode(178, "IND5"),
            new QCode(179, "IND6"),
            new QCode(180, "NSD7"),
            new QCode(181, "LGAWW"),
            new QCode(182, "STMW"),
            new QCode(183, "STDW"),
            new QCode(184, "SAS"),
            new QCode(185, "ADJ"),
            new QCode(186, "CALLL"),
            new QCode(187, "CALLV"),
            new QCode(188, "ATPB"),
            new QCode(189, "ATPW"),
            new QCode(190, "WCS"),
            new QCode(191, "JCS"),
            new QCode(192, "LDGB"),
            new QCode(193, "LDGW"),
            new QCode(194, "LGAB"),
            new QCode(195, "LGAW"),
            new QCode(196, "STGB"),
            new QCode(197, "STGW"),
            new QCode(198, "SGLB"),
            new QCode(199, "SGLW"),
            new QCode(200, "RET"),
            new QCode(201, "MMS2"),
            new QCode(202, "MES2"),
            new QCode(203, "LDTP"),
            new QCode(204, "JMPB"),
            new QCode(205, "JMPW"),
            new QCode(206, "JFB"),
            new QCode(207, "JFW"),
            new QCode(208, "JTB"),
            new QCode(209, "JTW"),
            new QCode(210, "JEQB"),
            new QCode(211, "JEQW"),
            new QCode(212, "JNEB"),
            new QCode(213, "JNEW"),
            new QCode(214, "IXP"),
            new QCode(215, "LDIB"),
            new QCode(216, "LDIW"),
            new QCode(217, "LIAB"),
            new QCode(218, "LIAW"),
            new QCode(219, "STIB"),
            new QCode(220, "STIW"),
            new QCode(221, "IXAB"),
            new QCode(222, "IXAW"),
            new QCode(223, "IXA1"),
            new QCode(224, "IXA2"),
            new QCode(225, "IXA3"),
            new QCode(226, "IXA4"),
            new QCode(227, "CCALL"),
            new QCode(228, "CENTER"),
            new QCode(229, "CRET"),
            new QCode(230, "EXCH"),
            new QCode(231, "EXCH2"),
            new QCode(232, "INCB"),
            new QCode(233, "INCW"),
            new QCode(234, "CALLXB"),
            new QCode(235, "CALLXW"),
            new QCode(236, "LDMC"),
            new QCode(237, "LDDC"),
            new QCode(238, "LDMW"),
            new QCode(239, "LDDW"),
            new QCode(240, "SETEXC"),
            new QCode(241, "LINE"),
            new QCode(242, "ENABLE"),
            new QCode(243, "QRAISE"),
            new QCode(244, "LDAP"),
            new QCode(245, "LGLB"),
            new QCode(246, "LGLW"),
            new QCode(247, "ZEROMEM"),
            new QCode(248, "EQNIL"),
            new QCode(249, "EVENT"),
            new QCode(250, "ROPS"),
            new QCode(251, "INCDDS"),
            new QCode(252, "LOPS"),
            new QCode(253, "KOPS")
            // 254..255 in common
        };

        /// <summary>
        /// The QCode redesign for Accent, Version 5.  This is "phase II", used
        /// in Accent S6 by Pascal v12?  Need to do more research...
        /// </summary>
        private static QCode[] _qCodesAccentV5 =
        {
            // 0..18 in common
            new QCode(19, "LDCN"),
            new QCode(20, "LDLC1"),
            new QCode(21, "LDLCM1"),
            new QCode(22, "LDLCB"),
            new QCode(23, "LDDC"),
            new QCode(24, "LSA"),
            new QCode(25, "QAND"),
            new QCode(26, "QOR"),
            new QCode(27, "QNOT"),
            new QCode(28, "QXOR"),
            new QCode(29, "EQUI"),
            new QCode(30, "NEQI"),
            new QCode(31, "LEQI"),
            new QCode(32, "LESI"),
            new QCode(33, "GEQI"),
            new QCode(34, "GTRI"),
            new QCode(35, "ABI"),
            new QCode(36, "ADI"),
            new QCode(37, "NGI"),
            new QCode(38, "SBI"),
            new QCode(39, "MPI"),
            new QCode(40, "DVI"),
            new QCode(41, "MODI"),
            new QCode(42, "CHK"),
            new QCode(43, "ROTSHI"),
            new QCode(44, "nCVTLI"),
            new QCode(45, "nCVTIL"),
            new QCode(46, "nADL"),
            new QCode(47, "nSBL"),
            new QCode(48, "nEQULONG"),
            new QCode(49, "nNEQLONG"),
            new QCode(50, "nLEQLONG"),
            new QCode(51, "nLESLONG"),
            new QCode(52, "nGEQLONG"),
            new QCode(53, "nGTRLONG"),
            new QCode(54, "EQNIL"),
            new QCode(55, "EXCH"),
            new QCode(56, "EXCH2"),
            new QCode(57, "REPL"),
            new QCode(58, "REPL2"),
            new QCode(59, "MMS"),
            new QCode(60, "MMS2"),
            new QCode(61, "MES"),
            new QCode(62, "MES2"),
            new QCode(63, "EXOP", true),
            new QCode(64, "LLLB"),
            new QCode(65, "LLLW"),
            new QCode(66, "SLLB"),
            new QCode(67, "SLLW"),
            new QCode(68, "LILB"),
            new QCode(69, "LILW"),
            new QCode(70, "SILB"),
            new QCode(71, "SILW"),
            new QCode(72, "LOLB"),
            new QCode(73, "LOLW"),
            new QCode(74, "SOLB"),
            new QCode(75, "SOLW"),
            new QCode(76, "LGLB"),
            new QCode(77, "LGLW"),
            new QCode(78, "SGLB"),
            new QCode(79, "SGLW"),
            new QCode(80, "LDLB"),
            new QCode(81, "LDLW"),
            new QCode(82, "LDL0"),
            new QCode(83, "LDL1"),
            new QCode(84, "LDL2"),
            new QCode(85, "LDL3"),
            new QCode(86, "LDL4"),
            new QCode(87, "LDL5"),
            new QCode(88, "LDL6"),
            new QCode(89, "LDL7"),
            new QCode(90, "LDL8"),
            new QCode(91, "LDL9"),
            new QCode(92, "LDL10"),
            new QCode(93, "LDL11"),
            new QCode(94, "LDL12"),
            new QCode(95, "LDL13"),
            new QCode(96, "LDL14"),
            new QCode(97, "LDL15"),
            new QCode(98, "LLAB"),
            new QCode(99, "LLAW"),
            new QCode(100, "STLB"),
            new QCode(101, "STLW"),
            new QCode(102, "STL0"),
            new QCode(103, "STL1"),
            new QCode(104, "STL2"),
            new QCode(105, "STL3"),
            new QCode(106, "STL4"),
            new QCode(107, "STL5"),
            new QCode(108, "STL6"),
            new QCode(109, "STL7"),
            new QCode(110, "LDIB"),
            new QCode(111, "LDIW"),
            new QCode(112, "LIAB"),
            new QCode(113, "LIAW"),
            new QCode(114, "STIB"),
            new QCode(115, "STIW"),
            new QCode(116, "LDOB"),
            new QCode(117, "LDOW"),
            new QCode(118, "LOAB"),
            new QCode(119, "LOAW"),
            new QCode(120, "STOB"),
            new QCode(121, "STOW"),
            new QCode(122, "LDGB"),
            new QCode(123, "LDGW"),
            new QCode(124, "LGAB"),
            new QCode(125, "LGAW"),
            new QCode(126, "STGB"),
            new QCode(127, "STGW"),
            new QCode(128, "LLLLB"),
            new QCode(129, "SLR0"),
            new QCode(130, "SLR1"),
            new QCode(131, "SLR2"),
            new QCode(132, "SLR3"),
            new QCode(133, "LLR0"),
            new QCode(134, "LLR1"),
            new QCode(135, "LLR2"),
            new QCode(136, "LLR3"),
            new QCode(137, "SIR0"),
            new QCode(138, "SIR1"),
            new QCode(139, "SIR2"),
            new QCode(140, "SIR3"),
            new QCode(141, "LIR0"),
            new QCode(142, "LIR1"),
            new QCode(143, "LIR2"),
            new QCode(144, "LIR3"),
            new QCode(145, "IXAB"),
            new QCode(146, "IXAW"),
            new QCode(147, "IXA1"),
            new QCode(148, "IXA2"),
            new QCode(149, "IXA3"),
            new QCode(150, "IXA4"),
            new QCode(151, "INCB"),
            new QCode(152, "INCW"),
            new QCode(153, "INDB"),
            new QCode(154, "INDW"),
            new QCode(155, "IND0"),
            new QCode(156, "IND1"),
            new QCode(157, "IND2"),
            new QCode(158, "IND3"),
            new QCode(159, "IND4"),
            new QCode(160, "IND5"),
            new QCode(161, "IND6"),
            new QCode(162, "IND7"),
            new QCode(163, "STIND"),
            new QCode(164, "LBLB"),
            new QCode(165, "LBL0"),
            new QCode(166, "STDW"),
            new QCode(167, "LDLIND"),
            new QCode(168, "LDB"),
            new QCode(169, "LDBLong"),
            new QCode(170, "LDBIND"),
            new QCode(171, "STB"),
            new QCode(172, "STBLong"),
            new QCode(173, "STBIND"),
            new QCode(174, "LDCH"),
            new QCode(175, "STCH"),
            new QCode(176, "IXP"),
            new QCode(177, "IXPLong"),
            new QCode(178, "LDP"),
            new QCode(179, "STPF"),
            new QCode(180, "SAS"),
            new QCode(181, "LXAB"),
            new QCode(182, "LXAW"),
            new QCode(183, "LXA2"),
            new QCode(184, "LXA3"),
            new QCode(185, "LXA4"),
            new QCode(186, "LBAR0B"),
            new QCode(187, "LBAR0W"),
            new QCode(188, "LBAR1B"),
            new QCode(189, "LBAR1W"),
            new QCode(190, "LBAR2B"),
            new QCode(191, "LBAR2W"),
            new QCode(192, "LBAR3B"),
            new QCode(193, "LBAR3W"),
            new QCode(194, "LBIR0B"),
            new QCode(195, "LBIR1B"),
            new QCode(196, "LBIR2B"),
            new QCode(197, "LBIR3B"),
            new QCode(198, "LBLR0B"),
            new QCode(199, "LBLR0W"),
            new QCode(200, "LBLR1B"),
            new QCode(201, "LBLR1W"),
            new QCode(202, "LBLR2B"),
            new QCode(203, "LBLR2W"),
            new QCode(204, "LBLR3B"),
            new QCode(205, "LBLR3W"),
            new QCode(206, "IXAR0B"),
            new QCode(207, "IXAR0W"),
            new QCode(208, "IXAR1B"),
            new QCode(209, "IXAR1W"),
            new QCode(210, "IXAR2B"),
            new QCode(211, "IXAR2W"),
            new QCode(212, "IXAR3B"),
            new QCode(213, "IXAR3W"),
            new QCode(214, "LXAR0B"),
            new QCode(215, "LXAR0W"),
            new QCode(216, "LXAR1B"),
            new QCode(217, "LXAR1W"),
            new QCode(218, "LXAR2B"),
            new QCode(219, "LXAR2W"),
            new QCode(220, "LXAR3B"),
            new QCode(221, "LXAR3W"),
            new QCode(222, "XJP"),
            new QCode(223, "JMPB"),
            new QCode(224, "JMPW"),
            new QCode(225, "JFB"),
            new QCode(226, "JFW"),
            new QCode(227, "JTB"),
            new QCode(228, "JTW"),
            new QCode(229, "JEQB"),
            new QCode(230, "JEQW"),
            new QCode(231, "JNEB"),
            new QCode(232, "JNEW"),
            new QCode(233, "LDRET1"),
            new QCode(234, "LDRET2"),
            new QCode(235, "CCALL"),
            new QCode(236, "CENTER"),
            new QCode(237, "CRET"),
            new QCode(238, "CALLXB"),
            new QCode(239, "CALLL"),
            new QCode(240, "CALLV"),
            new QCode(241, "RET"),
            new QCode(242, "EXITT"),
            new QCode(243, "EXGO"),
            new QCode(244, "ATPB"),
            new QCode(245, "ATPW"),
            new QCode(246, "LDAP"),
            new QCode(247, "LDTP"),
            new QCode(248, "EVENT"),
            new QCode(249, "WCS"),
            new QCode(250, "JCS"),
            new QCode(251, "GOTOOVL"),
            new QCode(252, "KOPS", true),
            new QCode(253, "NOOP")
            // 254..255 in common
        };

        /// <summary>
        /// Second byte of the Accent V5 EXOP extended opcodes.
        /// </summary>
        private static QCode[] _qCodesAccentV5exops =
        {
            new QCode(0, "exUndef0"),
            new QCode(1, "exUndef1"),
            new QCode(2, "exUndef2"),
            new QCode(3, "NGL"),
            new QCode(4, "exUndef4"),
            new QCode(5, "MPL"),
            new QCode(6, "DVL"),
            new QCode(7, "MODL"),
            new QCode(8, "ABL"),
            new QCode(9, "exUndef9"),
            new QCode(10, "exUndef10"),
            new QCode(11, "exUndef11"),
            new QCode(12, "exUndef12"),
            new QCode(13, "exUndef13"),
            new QCode(14, "exUndef14"),
            new QCode(15, "LBITS"),
            new QCode(16, "LBNOT"),
            new QCode(17, "LBAND"),
            new QCode(18, "LBOR"),
            new QCode(19, "LBXOR"),
            new QCode(20, "exCHKLong"),
            new QCode(21, "exTNCRI"),
            new QCode(22, "exFLTIR"),
            new QCode(23, "exADR"),
            new QCode(24, "exNGR"),
            new QCode(25, "exSBR"),
            new QCode(26, "exMPR"),
            new QCode(27, "exDVR"),
            new QCode(28, "exRNDRI"),
            new QCode(29, "exABR"),
            new QCode(30, "exEQUReal"),
            new QCode(31, "exNEQReal"),
            new QCode(32, "exLEQReal"),
            new QCode(33, "exLESReal"),
            new QCode(34, "exGEQReal"),
            new QCode(35, "exGTRReal"),
            new QCode(36, "exTNCQL"),
            new QCode(37, "exFLTLQ"),
            new QCode(38, "exADDQ"),
            new QCode(39, "exNEGQ"),
            new QCode(40, "exSUBQ"),
            new QCode(41, "exMULQ"),
            new QCode(42, "exDIVQ"),
            new QCode(43, "exRNDQL"),
            new QCode(44, "exABSQ"),
            new QCode(45, "exEQUQ"),
            new QCode(46, "exNEQQ"),
            new QCode(47, "exLEQQ"),
            new QCode(48, "exLESQ"),
            new QCode(49, "exGEQQ"),
            new QCode(50, "exGTRQ"),
            new QCode(51, "exTNCRL"),
            new QCode(52, "exFLTLR"),
            new QCode(53, "exCVQR"),
            new QCode(54, "exCVRQ"),
            new QCode(55, "exRNDRL"),
            new QCode(56, "exLDQ"),
            new QCode(57, "exSTQ"),
            new QCode(58, "exEXCHQ"),
            new QCode(59, "exPERMD"),
            new QCode(60, "exJLK"),
            new QCode(61, "exJMS"),
            new QCode(62, "exEQUStr"),
            new QCode(63, "exNEQStr"),
            new QCode(64, "exLEQStr"),
            new QCode(65, "exLESStr"),
            new QCode(66, "exGEQStr"),
            new QCode(67, "exGTRStr"),
            new QCode(68, "exEQUByt"),
            new QCode(69, "exNEQByt"),
            new QCode(70, "exLEQByt"),
            new QCode(71, "exLESByt"),
            new QCode(72, "exGEQByt"),
            new QCode(73, "exGTRByt"),
            new QCode(74, "exEQUPowr"),
            new QCode(75, "exNEQPowr"),
            new QCode(76, "exLEQPowr"),
            new QCode(77, "exSGS"),
            new QCode(78, "exGEQPowr"),
            new QCode(79, "exSRS"),
            new QCode(80, "exINN"),
            new QCode(81, "exUNI"),
            new QCode(82, "exQINT"),
            new QCode(83, "exDIF"),
            new QCode(84, "exADJ"),
            new QCode(85, "exEQUWord"),
            new QCode(86, "exNEQWord"),
            new QCode(87, "exRASTOP"),
            new QCode(88, "exSTRROP"),
            new QCode(89, "exLINE"),
            new QCode(90, "exMVBB"),
            new QCode(91, "exMVBW"),
            new QCode(92, "exMOVB"),
            new QCode(93, "exMOVW"),
            new QCode(94, "exSTMW"),
            new QCode(95, "exLDMC"),
            new QCode(96, "exLDMW"),
            new QCode(97, "exSETEXC"),
            new QCode(98, "exENABLE"),
            new QCode(99, "exQRAISE"),
            new QCode(100, "exZEROMEM"),
            new QCode(101, "exINCDDS"),
            new QCode(102, "exSTRTIO"),
            new QCode(103, "exLVRD"),
            new QCode(104, "exLBIR0W"),
            new QCode(105, "exLBIR1W"),
            new QCode(106, "exLBIR2W"),
            new QCode(107, "exLBIR3W"),
            new QCode(108, "exLBQR0B"),
            new QCode(109, "exLBQR0W"),
            new QCode(110, "exLBQR1B"),
            new QCode(111, "exLBQR1W"),
            new QCode(112, "exLBQR2B"),
            new QCode(113, "exLBQR2W"),
            new QCode(114, "exLBQR3B"),
            new QCode(115, "exLBQR3W"),
            new QCode(116, "exIXAR01"),
            new QCode(117, "exIXAR02"),
            new QCode(118, "exIXAR03"),
            new QCode(119, "exIXAR04"),
            new QCode(120, "exIXAR11"),
            new QCode(121, "exIXAR12"),
            new QCode(122, "exIXAR13"),
            new QCode(123, "exIXAR14"),
            new QCode(124, "exIXAR21"),
            new QCode(125, "exIXAR22"),
            new QCode(126, "exIXAR23"),
            new QCode(127, "exIXAR24"),
            new QCode(128, "exIXAR31"),
            new QCode(129, "exIXAR32"),
            new QCode(130, "exIXAR33"),
            new QCode(131, "exIXAR34"),
            new QCode(132, "exLXAR01"),
            new QCode(133, "exLXAR02"),
            new QCode(134, "exLXAR03"),
            new QCode(135, "exLXAR04"),
            new QCode(136, "exLXAR11"),
            new QCode(137, "exLXAR12"),
            new QCode(138, "exLXAR13"),
            new QCode(139, "exLXAR14"),
            new QCode(140, "exLXAR21"),
            new QCode(141, "exLXAR22"),
            new QCode(142, "exLXAR23"),
            new QCode(143, "exLXAR24"),
            new QCode(144, "exLXAR31"),
            new QCode(145, "exLXAR32"),
            new QCode(146, "exLXAR33"),
            new QCode(147, "exLXAR34"),
            new QCode(148, "exPop1"),
            new QCode(149, "exPop2"),
            new QCode(150, "exNDSTLB"),
            new QCode(151, "exNDSTLW"),
            new QCode(152, "exNDSLLB"),
            new QCode(153, "exNDSLLW"),
            new QCode(154, "exNDSTGB"),
            new QCode(155, "exNDSTGW"),
            new QCode(156, "exNDSGLB"),
            new QCode(157, "exNDSGLW"),
            new QCode(158, "exNDSIR0"),
            new QCode(159, "exNDSIR1"),
            new QCode(160, "exNDSIR2"),
            new QCode(161, "exNDSIR3"),
            new QCode(162, "exNDSLR0"),
            new QCode(163, "exNDSLR1"),
            new QCode(164, "exNDSLR2"),
            new QCode(165, "exNDSLR3"),
            new QCode(166, "exMathMODI"),
            new QCode(167, "exMathMODL"),
            // 168..238 undefined
            new QCode(239, "SvRet1"),
            new QCode(240, "exUndef240"),
            new QCode(241, "SvReg2"),
            new QCode(242, "exUndef242"),
            new QCode(243, "CVTCL"),
            new QCode(244, "exUndef244"),
            new QCode(245, "CVTCI"),
            new QCode(246, "LOPSHI"),
            new QCode(247, "DECREG3"),
            new QCode(248, "DECREG2"),
            new QCode(249, "DECREG1"),
            new QCode(250, "DECREG0"),
            new QCode(251, "INCREG3"),
            new QCode(252, "INCREG2"),
            new QCode(253, "INCREG1"),
            new QCode(254, "INCREG0"),
            new QCode(255, "EXREFILLOP")
        };

        /// <summary>
        /// Second byte of the Accent V5 KOPS Kernel opcodes.
        /// </summary>
        private static QCode[] _qCodesAccentV5kops =
        {
            new QCode(0, "KSVRETURN"),
            new QCode(1, "KCURPROCESS"),
            new QCode(2, "KHASH"),
            new QCode(3, "KSETSOFT"),
            new QCode(4, "KCLOCK"),
            new QCode(5, "KGETCB"),
            new QCode(6, "KSETVMTABLES"),
            new QCode(9, "KSVCALL"),
            new QCode(10, "KADDTOQUEUE"),
            new QCode(11, "KREMOVEFROMQUEUE"),
            new QCode(12, "KWAKEUP"),
            new QCode(13, "KSLEEP"),
            new QCode(14, "KUNBLOCK"),
            new QCode(15, "KBLOCK"),
            new QCode(16, "KLOCKSVC"),
            new QCode(17, "KUNLOCKSVC"),
            new QCode(18, "KSEARCH"),
            new QCode(19, "KVPENTER"),
            new QCode(20, "KVPREMOVE"),
            new QCode(21, "KSEARCHADDR"),
            new QCode(22, "KSEARCHPV"),
            new QCode(23, "KCOPYPAGE")
        };
    }
}

