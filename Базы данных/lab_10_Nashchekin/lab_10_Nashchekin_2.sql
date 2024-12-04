USE master;
GO

USE lab10;
GO


-- Для грязного чтения
BEGIN TRANSACTION
	UPDATE Galaxy SET Type = 14
	WHERE NGCNumber = 'NGC13'
	WAITFOR DELAY '00:00:04';
	SELECT * FROM sys.dm_tran_locks;
ROLLBACK TRANSACTION;
GO


-- Для невоспроизводимого чтения
BEGIN TRANSACTION 
	UPDATE Galaxy SET Type = 5, ApparentMagnitude = 8
	WHERE NGCNumber = 'NGC13';
	SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;
GO


-- Для фантомного чтения
BEGIN TRANSACTION 
	INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1612', 6, 4, 11, 5);
	SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;
GO



