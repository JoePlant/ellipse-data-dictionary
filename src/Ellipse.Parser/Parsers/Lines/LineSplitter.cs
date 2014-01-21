using System;
using System.Collections.Generic;

namespace Ellipse.DataDictionary.Parsers.Lines
{
    public class LineSplitter : ILineSplitter
    {
        private readonly LineParser lineParser;
        private readonly string[] splitMarker;
        private string selectMarker;
        private readonly List<int> selectIndexs = new List<int>();
        private readonly List<int> ignoreIndexs = new List<int>();
        private bool ignoreFollowing = false;
        private bool selectFollowing = false;
        private string followingContext = "";
        private string joinMarker;

        public LineSplitter(LineParser lineParser, string marker)
        {
            this.lineParser = lineParser;
            splitMarker = new [] {marker};
            selectMarker = null;

        }

        public ILineSplitter Find(string marker)
        {
            selectMarker = marker;
            return this;
        }

        public ILineSplitter Ignore(params int[] offsets)
        {
            foreach (int offset in offsets)
            {
                ignoreIndexs.Add(offset);
            }
            followingContext = "Ignore";
            return this; 
        }

        public ILineSplitter AndFollowing()
        {
            if (followingContext == "Ignore")
            {
                ignoreFollowing = true;
            }
            
            if (followingContext == "Select")
            {
                selectFollowing = true;
            }
            followingContext = "";
            return this;
        }

        public ILineSplitter Select(params int[] offsets)
        {
            foreach (int offset in offsets)
            {
                selectIndexs.Add(offset);
            }
            followingContext = "Select";
            return this;
        }

        public ILineParser Join(string marker)
        {
            joinMarker = marker;
            return lineParser;
        }
        
        public string Parse(int lineNo, string line)
        {
            if (line.Contains(splitMarker[0]))
            {
                string[] parts = line.Split(splitMarker, StringSplitOptions.None);
                List<string> results = new List<string>();
                int select = 0;
                if (selectMarker != null)
                {
                    int pos = 0;
                    foreach (string part in parts)
                    {
                        if (string.Compare(part, selectMarker, StringComparison.InvariantCulture) == 0)
                        {
                            select = pos;
                            break;
                        }
                        pos++;
                    }
                }
                if (selectIndexs.Count > 0)
                {
                    int cursor = 0;
                    foreach (int offset in selectIndexs)
                    {
                        int i = select + offset;
                        if (i < 0 || i >= parts.Length)
                        {
                            return line;
                        }
                        cursor = i;
                        results.Add(parts[i]);
                    }
                    if (selectFollowing)
                    {
                        for (int i = cursor+1; i < parts.Length; i++)
                        {
                            results.Add(parts[i]);
                        }
                    }
                }
                else
                {
                    List<int> ignoreParts = new List<int>();
                    if (ignoreIndexs.Count > 0)
                    {
                        int cursor = 0;
                        foreach (int offset in ignoreIndexs)
                        {
                            int i = select + offset;

                            if (i < 0 || i >= parts.Length)
                            {
                                return line;
                            }
                            cursor = i;
                            ignoreParts.Add(i);
                        }
                        if (ignoreFollowing)
                        {
                            for (int i = cursor + 1; i < parts.Length; i++)
                            {
                                ignoreParts.Add(i);
                            }
                        }
                    }
                    int pos = 0;
                    foreach (string part in parts)
                    {
                        if (!ignoreParts.Contains(pos))
                        {
                            results.Add(part);
                        }
                        pos++;
                    }
                }

                if (results.Count > 0)
                {
                    return string.Join(joinMarker, results.ToArray());
                }

            }
            return line;
        }
    }
}