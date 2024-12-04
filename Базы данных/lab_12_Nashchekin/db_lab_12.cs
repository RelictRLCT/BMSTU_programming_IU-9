using System;
using System.ComponentModel;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
using Microsoft.Data.SqlClient;
using Microsoft.IdentityModel.Protocols;

namespace db_lab_12 {
    class ConnLevel {
        static string UpdateQuery = "UPDATE Galaxy SET ApparentMagnitude = @ApparentMagnitude WHERE NGCNumber = @NGCNumber";
        static string InsertQuery =  "INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)" +  
            "VALUES (@NGCNumber,@Type,@Size, @Constellation, @ApparentMagnitude)";
        static string DeleteQuery = "DELETE FROM Galaxy WHERE NGCNumber = @NGCNumber";
        
        
        public static void SelectFrom(string tableName) {
            try {
                using (SqlConnection conn = new SqlConnection(ConfigurationManager.AppSettings.Get("connString"))) {
                    conn.Open();

                    SqlCommand newComm = conn.CreateCommand();
                    newComm.Connection = conn;
                    newComm.CommandText = "SELECT * FROM " + tableName;

                    using (SqlDataReader reader = newComm.ExecuteReader()) {

                        for (int i = 0; i < reader.FieldCount; i++)
                        {
                            Console.Write(reader.GetName(i) + " ");
                        }

                        Console.WriteLine();
                        while (reader.Read())
                        {
                            for (int i = 0; i < reader.FieldCount; i++)
                            {
                                Console.Write(reader.GetValue(i).ToString() + "\t");
                            }

                            Console.WriteLine();
                        }
                    }
                }
            }
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }
        
        public static void InsertGalaxy(string NGCNumber, int Type, int Size, int Constellation, float ApparentMagnitude) {
            try {
                using (SqlConnection conn = new SqlConnection(ConfigurationManager.AppSettings.Get("connString"))) {
                    conn.Open();

                    SqlCommand newComm = conn.CreateCommand();
                    newComm.Connection = conn;
                    newComm.CommandText = InsertQuery;

                    SqlParameter[] parametes = new SqlParameter[5];
                    parametes[0] = new SqlParameter();
                    parametes[0].ParameterName = "@NGCNumber";
                    parametes[0].Value = NGCNumber;
                    parametes[0].SqlDbType = SqlDbType.VarChar;

                    parametes[1] = new SqlParameter();
                    parametes[1].ParameterName = "@Type";
                    parametes[1].Value = Type;
                    parametes[1].SqlDbType = SqlDbType.Int;

                    parametes[2] = new SqlParameter();
                    parametes[2].ParameterName = "@Size";
                    parametes[2].Value = Size;
                    parametes[2].SqlDbType = SqlDbType.Int;

                    parametes[3] = new SqlParameter();
                    parametes[3].ParameterName = "@Constellation";
                    parametes[3].Value = Constellation;
                    parametes[3].SqlDbType = SqlDbType.Int;

                    parametes[4] = new SqlParameter();
                    parametes[4].ParameterName = "@ApparentMagnitude";
                    parametes[4].Value = ApparentMagnitude;
                    parametes[4].SqlDbType = SqlDbType.Float;

                    newComm.Parameters.AddRange(parametes);
                    Console.WriteLine("INSERT: {0} строк затронуто ", newComm.ExecuteNonQuery());
                }
            }
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }
        
        
        public static void UpdateGalaxyAppMag(string NGCNumber, float ApparentMagnitude) {
            try {
                using (SqlConnection conn = new SqlConnection(ConfigurationManager.AppSettings.Get("connString"))) {
                    conn.Open();
                    SqlCommand newComm = conn.CreateCommand();
                    newComm.Connection = conn;
                    newComm.CommandText = UpdateQuery;

                    SqlParameter[] parametes = new SqlParameter[2];
                    parametes[0] = new SqlParameter();
                    parametes[0].ParameterName = "@NGCNumber";
                    parametes[0].Value = NGCNumber;
                    parametes[0].SqlDbType = SqlDbType.VarChar;

                    parametes[1] = new SqlParameter();
                    parametes[1].ParameterName = "@ApparentMagnitude";
                    parametes[1].Value = ApparentMagnitude;
                    parametes[1].SqlDbType = SqlDbType.Float;

                    newComm.Parameters.AddRange(parametes);
                    Console.WriteLine("UPDATE: {0} строк затронуто ", newComm.ExecuteNonQuery());
                }
            }
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }
        
        public static void DeleteGalaxy(string NGCNumber) {
            try {
                using (SqlConnection conn = new SqlConnection(ConfigurationManager.AppSettings.Get("connString"))) {
                    conn.Open();
                    SqlCommand newComm = conn.CreateCommand();
                    newComm.Connection = conn;
                    newComm.CommandText = DeleteQuery;

                    SqlParameter param = new SqlParameter();
                    param.ParameterName = "@NGCNumber";
                    param.Value = NGCNumber;
                    param.SqlDbType = SqlDbType.VarChar;

                    newComm.Parameters.Add(param);
                    Console.WriteLine("DELETE: {0} строк затронуто ", newComm.ExecuteNonQuery());
                }
            }
            catch (Exception ex) {
                Console.Write(ex.Message);
            }
        }
    }

    
    class DisConnLevel {
        private static SqlConnection conn;
        private static DataSet dataset;
        private static SqlDataAdapter dataAdapter;
        
        static string UpdateQuery = "UPDATE Galaxy SET ApparentMagnitude = @ApparentMagnitude WHERE NGCNumber = @NGCNumber";
        static string InsertQuery = "INSERT INTO Galaxy (NGCNumber, Type, Size, Constellation, ApparentMagnitude) " +
                                    "VALUES (@NGCNumber, @Type, @Size, @Constellation, @ApparentMagnitude); " +
                                    "SET @GalaxyID = SCOPE_IDENTITY();";
        static string DeleteQuery = "DELETE FROM Galaxy WHERE NGCNumber = @NGCNumber";
        public DisConnLevel() {
            
            try {
                conn = new SqlConnection(ConfigurationManager.AppSettings.Get("connString"));
                conn.Open();
                
                dataset = new DataSet();
                dataAdapter = new SqlDataAdapter("SELECT * FROM Galaxy", conn);
                dataAdapter.Fill(dataset, "Galaxy");
                
                
                // INSERT
                SqlCommand newComm = conn.CreateCommand();
                newComm.Connection = conn;
                newComm.CommandText = InsertQuery;

                SqlParameter[] parameters = new SqlParameter[6];
                parameters[0] = new SqlParameter();
                parameters[0].ParameterName = "@NGCNumber";
                parameters[0].SourceColumn = "NGCNumber";
                parameters[0].SqlDbType = SqlDbType.VarChar;

                parameters[1] = new SqlParameter();
                parameters[1].ParameterName = "@Type";
                parameters[1].SourceColumn = "Type";
                parameters[1].SqlDbType = SqlDbType.Int;

                parameters[2] = new SqlParameter();
                parameters[2].ParameterName = "@Size";
                parameters[2].SourceColumn = "Size";
                parameters[2].SqlDbType = SqlDbType.Int;

                parameters[3] = new SqlParameter();
                parameters[3].ParameterName = "@Constellation";
                parameters[3].SourceColumn = "Constellation";
                parameters[3].SqlDbType = SqlDbType.Int;
                
                parameters[4] = new SqlParameter();
                parameters[4].ParameterName = "@ApparentMagnitude";
                parameters[4].SourceColumn = "ApparentMagnitude";
                parameters[4].SqlDbType = SqlDbType.Float;
                
                parameters[5] = new SqlParameter();
                parameters[5].ParameterName = "@GalaxyID";
                parameters[5].SourceColumn = "GalaxyID";
                parameters[5].SqlDbType = SqlDbType.Int;
                parameters[5].Direction = ParameterDirection.Output;

                newComm.Parameters.AddRange(parameters);
                dataAdapter.InsertCommand = newComm;
                dataAdapter.InsertCommand.Connection = conn;
                
                
                // UPDATE
                newComm = conn.CreateCommand();
                newComm.Connection = conn;
                newComm.CommandText = UpdateQuery;
                
                parameters = new SqlParameter[2];
                parameters[0] = new SqlParameter();
                parameters[0].ParameterName = "@NGCNumber";
                parameters[0].SourceColumn = "NGCNumber";
                parameters[0].SqlDbType = SqlDbType.VarChar;

                parameters[1] = new SqlParameter();
                parameters[1].ParameterName = "@ApparentMagnitude";
                parameters[1].SourceColumn = "ApparentMagnitude";
                parameters[1].SqlDbType = SqlDbType.Float;

                newComm.Parameters.AddRange(parameters);
                dataAdapter.UpdateCommand = newComm;
                dataAdapter.UpdateCommand.Connection = conn;
                
                // DELETE
                newComm = new SqlCommand();
                newComm.Connection = conn;
                newComm.CommandText = DeleteQuery;
                
                SqlParameter param = new SqlParameter();
                param.ParameterName = "@NGCNumber";
                param.SourceColumn = "NGCNumber";
                param.SqlDbType = SqlDbType.VarChar;

                newComm.Parameters.Add(param);
                dataAdapter.DeleteCommand = newComm;
                dataAdapter.DeleteCommand.Connection = conn;
                
                conn.Close();
            }
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }
        
        public void SelectFrom(string tableName) {
            try {
                DataTableReader tableReader = dataset.Tables[tableName].CreateDataReader();
                int countCols = tableReader.FieldCount;
                for (int i = 0; i < countCols; i++) {
                    Console.Write(tableReader.GetName(i) + " ");
                }
                Console.WriteLine();
                while (tableReader.Read()) {
                    for (int i = 0; i < countCols; i++) {
                        Console.Write(tableReader.GetValue(i).ToString() + "\t");
                    }
                    Console.WriteLine();
                }
                Console.WriteLine();
                tableReader.Close();
            }
            catch (Exception e) {
                Console.WriteLine(e);
            }
        }
        
        public void InsertGalaxy(string NGCNumber, int Type, int Size, int Constellation, float ApparentMagnitude) {
            try {
                DataRow dataRow = dataset.Tables["Galaxy"].NewRow();
                dataRow["NGCNumber"] = NGCNumber;
                dataRow["Type"] = Type;
                dataRow["Size"] = Size;
                dataRow["Constellation"] = Constellation;
                dataRow["ApparentMagnitude"] = ApparentMagnitude;
                dataset.Tables["Galaxy"].Rows.Add(dataRow);
            }
            catch (Exception e) {
                Console.WriteLine(e);
            }
        }

        public void UpdateGalaxy(string NGCNumber, float ApparentMagnitude) {
            try {
                int countRows = dataset.Tables["Galaxy"].Rows.Count;
                
                for (int i = 0; i < countRows; i++) {
                    if (dataset.Tables["Galaxy"].Rows[i]["NGCNumber"].ToString() == NGCNumber) {
                        dataset.Tables["Galaxy"].Rows[i]["ApparentMagnitude"] = ApparentMagnitude;
                    }
                }
            }
            catch (Exception e) {
                Console.WriteLine(e);
            }
        }
        
        public void DeleteGalaxy(string NGCNumber) {
            try {
                int countRows = dataset.Tables["Galaxy"].Rows.Count;
                for (int i = 0; i < countRows ; i++) {
                    if (dataset.Tables["Galaxy"].Rows[i]["NGCNumber"].ToString() == NGCNumber) {
                        dataset.Tables["Galaxy"].Rows[i].Delete();
                        break;
                    }
                }
            }
            catch (Exception e) {
                Console.WriteLine(e);
            }
        }

        public void Commit() {
            try {
                conn.Open();
                int numAffected = dataAdapter.Update(dataset, "Galaxy");
                Console.WriteLine("{0} строк затронуто ", numAffected);
                conn.Close();
            } 
            catch (Exception ex) {
                Console.WriteLine(ex.Message);
            }
        }
    }
    
    
    class db_lab_12 {   
        static void Main() {
            repeat_input:
            Console.Write("Связный/несвязный уровень (1/2): ");
            int mode = Convert.ToInt32(Console.ReadLine());
            if (mode == 1) {
                ConnLevel.SelectFrom("Galaxy");
                ConnLevel.InsertGalaxy("NGC6543",2,11, 2, 10);
                ConnLevel.SelectFrom("Galaxy");
                ConnLevel.UpdateGalaxyAppMag("NGC6543",16);
                ConnLevel.SelectFrom("Galaxy");
                ConnLevel.DeleteGalaxy("NGC6543");
                ConnLevel.SelectFrom("Galaxy");    
            } else if (mode == 2) {
                DisConnLevel dl = new DisConnLevel();
                dl.SelectFrom("Galaxy");
                dl.InsertGalaxy("NGC6543",2,11, 2, 10);
                dl.SelectFrom("Galaxy");
                dl.UpdateGalaxy("NGC6543",4);
                dl.Commit();
                dl.SelectFrom("Galaxy");
                dl.DeleteGalaxy("NGC6543");
                dl.Commit();
                dl.SelectFrom("Galaxy");
            } else {
                Console.WriteLine("Неправильный режим ");
                goto repeat_input;
            }
        }
    }
}