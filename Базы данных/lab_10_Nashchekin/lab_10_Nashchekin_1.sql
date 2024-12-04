-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab10') IS NOT NULL
DROP DATABASE lab10;
GO

-- Создание базы
CREATE DATABASE lab10
	ON (
		NAME = lab10_dat, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_10/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab10_log, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_10/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab10;
GO

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES 
		('NGC13', 4, 11, 22, 11),
		('NGC22', 5, 12, 23, 12)
GO;

-- 1. Грязное чтение
-- незавершенное чтение
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
BEGIN TRANSACTION;
	SELECT * FROM Galaxy;
	WAITFOR DELAY '00:00:04';
	SELECT * FROM Galaxy;
COMMIT TRANSACTION;


-- завершенное чтение
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
BEGIN TRANSACTION;
	SELECT * FROM Galaxy;
	WAITFOR DELAY '00:00:04';
	SELECT * FROM Galaxy;
    SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;


-- 2. Невоспроизводимое чтение
-- воспроизводимое чтение
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
BEGIN TRANSACTION;
	SELECT * FROM Galaxy;
	WAITFOR DELAY '00:00:04';
	SELECT * FROM Galaxy;
    SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;


-- 3. Фантомное чтение
-- сериализация
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
BEGIN TRANSACTION;
	SELECT * FROM Galaxy;
	WAITFOR DELAY '00:00:04';
	SELECT * FROM Galaxy;
    SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;




