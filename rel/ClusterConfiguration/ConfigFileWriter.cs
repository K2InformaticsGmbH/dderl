using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Configuration.Install;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;

namespace ClusterConfiguration
{
    [RunInstaller(true)]
    public class ConfigFileWriter : Installer
    {
        [System.Security.Permissions.SecurityPermission(System.Security.Permissions.SecurityAction.Demand)]
        public override void Install(System.Collections.IDictionary stateSaver)
        {
            base.Install(stateSaver);
            string installationPath = this.Context.Parameters["targetdir"];

            //Vm args
            string nodeName = this.Context.Parameters["nodeName"];
            string cookie = this.Context.Parameters["cookie"];

            //Sys config values
            char[] delimiterChars = {':'};
            string[] dderlIpPort = this.Context.Parameters["dderlInterface"].Split(delimiterChars);
            string dderlIp = "0.0.0.0";
            string dderlPort = "8443";
            if (dderlIpPort.Length == 2)
            {
                dderlIp = dderlIpPort[0];
                dderlPort = dderlIpPort[1];
            }
            string mnesiaNodeType = this.Context.Parameters["nodeType"];
            string imemSchema = this.Context.Parameters["imemSchema"];
            string clusterManager = this.Context.Parameters["clusterManager"];

            string[] imemIpPort = this.Context.Parameters["imemInterface"].Split(delimiterChars);
            string imemIp = "0.0.0.0";
            string imemPort = "8126";
            if (imemIpPort.Length == 2)
            {
                imemIp = imemIpPort[0];
                imemPort = imemIpPort[1];
            }

            var releasePath = Path.Combine(installationPath, "releases\\1.0.7");
            var vmArgsPath = Path.Combine(releasePath, "vm.args");
            var sysConfigPath = Path.Combine(releasePath, "sys.config");

            writeVmArgs(vmArgsPath, nodeName, cookie);
            writeSysConfig(sysConfigPath, dderlIp, dderlPort, mnesiaNodeType, imemSchema, clusterManager, imemIp, imemPort);

            
        }

        private void writeSysConfig(string sysConfigPath, string dderlIp, string dderlPort, string mnesiaNodeType, string imemSchema, string clusterManager, string imemIp, string imemPort)
        {
            string line;
            System.IO.StreamReader file = new System.IO.StreamReader(sysConfigPath);
            List<string> linesList = new List<string>();
            while ((line = file.ReadLine()) != null)
            {
                if (line.Contains("interface"))
                {
                    line = "    {interface, \"" + dderlIp + "\" },";
                }
                else if (line.Contains("{port,"))
                {
                    line = "    {port, " + dderlPort + " }";
                }
                else if (line.Contains("mnesia_node_type"))
                {
                    line = "    {mnesia_node_type, " + mnesiaNodeType + " },";
                }
                else if (line.Contains("mnesia_schema_name"))
                {
                    line = "    {mnesia_schema_name, '" + imemSchema + "' },";
                }
                else if (line.Contains("erl_cluster_mgrs"))
                {
                    line = "    {erl_cluster_mgrs, ['" + clusterManager + "'] },";
                }
                else if (line.Contains("tcp_ip"))
                {
                    line = "    {tcp_ip, \"" + imemIp + "\" },";
                }
                else if (line.Contains("tcp_port"))
                {
                    line = "    {tcp_port, " + imemPort +" },";
                }
                else if (line.Contains("error.log\""))
                {
                    line = "                    {lager_file_backend, [{file, \"./logs/error.log\"},";
                }
                else if (line.Contains("console.log"))
                {
                    line = "                    {lager_file_backend, [{file, \"./logs/console.log\"},";
                }
                else if (line.Contains("crash.log"))
                {
                    line = "        {crash_log, \"./logs/crash.log\"},";
                }
                linesList.Add(line);
            }
            file.Close();

            File.WriteAllLines(sysConfigPath, linesList.ToArray());
        }

        private void writeVmArgs(string vmArgsPath, string nodeName, string cookie)
        {
            string line;
            System.IO.StreamReader file = new System.IO.StreamReader(vmArgsPath);
            List<string> linesList = new List<string>();
            while ((line = file.ReadLine()) != null)
            {
                if (line.StartsWith("-name"))
                {
                    line = "-name " + nodeName;
                }
                else if (line.StartsWith("-setcookie"))
                {
                    line = "-setcookie " + cookie;
                }
                linesList.Add(line);
            }
            file.Close();

            File.WriteAllLines(vmArgsPath, linesList.ToArray());
        }

        [System.Security.Permissions.SecurityPermission(System.Security.Permissions.SecurityAction.Demand)]
        public override void Uninstall(System.Collections.IDictionary savedState)
        {
            base.Uninstall(savedState);
            //MessageBox.Show(" Uninstall ");

        }

        [System.Security.Permissions.SecurityPermission(System.Security.Permissions.SecurityAction.Demand)]
        public override void Rollback(System.Collections.IDictionary savedState)
        {
            base.Rollback(savedState);
            //MessageBox.Show(" RollBack ");

        }

        [System.Security.Permissions.SecurityPermission(System.Security.Permissions.SecurityAction.Demand)]
        public override void Commit(System.Collections.IDictionary savedState)
        {
            base.Commit(savedState);
            //MessageBox.Show(" Commit ");

        }
    }
}
