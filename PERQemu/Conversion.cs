

namespace PERQemu
{
    public static class Conversion
    {
        /// <summary>
        /// Conversion from millseconds to nanoseconds
        /// </summary>
        public static readonly ulong MsecToNsec = 1000000;

        /// <summary>
        /// Conversion from nanoseconds to milliseconds
        /// </summary>
        public static readonly double NsecToMsec = 0.000001;

        /// <summary>
        /// Conversion from microseconds to nanoseconds
        /// </summary>
        public static readonly ulong UsecToNsec = 1000;

        /// <summary>
        /// Conversion from microseconds to seconds
        /// </summary>
        public static readonly double UsecToSec = 0.000001;
    }
}
